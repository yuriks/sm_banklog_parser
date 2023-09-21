use std::fmt::{Debug, Display, Formatter};
use std::fs::File;

use anyhow::{Context, Result};
use glob::glob;
use itertools::Itertools;
use serde::Deserialize;

use crate::label::LabelType;
use crate::{Addr, Bank};

#[derive(Debug, PartialEq, Deserialize)]
pub struct StructField {
    pub name: String,
    pub offset: u64,
    pub length: u64,
    #[serde(flatten)]
    pub type_: LabelType,
    pub db: Option<Bank>,
}

#[derive(Debug, PartialEq, Deserialize)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
}

impl Struct {
    pub fn size(&self) -> u64 {
        let Some(last_field) = self.fields.last() else {
            return 0;
        };
        last_field.offset + last_field.length
    }

    pub fn field_at_offset(&self, offset: u64) -> Option<&StructField> {
        self.fields.iter().find(|f| f.offset == offset)
    }
}

#[derive(Clone, Debug, PartialEq, Deserialize)]
pub struct Label {
    pub addr: OverrideAddr,
    pub name: Option<String>,
    // Needs to be an Option because default doesn't work with flatten:
    // https://github.com/serde-rs/serde/issues/1626
    #[serde(default, flatten)]
    pub type_: Option<LabelType>,
}

#[derive(Copy, Clone, Debug, PartialEq, Deserialize)]
#[serde(untagged)]
pub enum OverrideAddr {
    Address(Addr),
    Range(Addr, Addr),
}

impl OverrideAddr {
    fn as_range(self) -> (Addr, Addr) {
        match self {
            OverrideAddr::Address(x) => (x, x),
            OverrideAddr::Range(a, b) => (a, b),
        }
    }

    fn overlaps(&self, o: &Self) -> bool {
        let (start1, end1) = self.as_range();
        let (start2, end2) = o.as_range();

        start1 <= end2 && end1 >= start2
    }

    pub fn first(self) -> Addr {
        match self {
            OverrideAddr::Address(x) | OverrideAddr::Range(x, _) => x,
        }
    }

    fn length(self) -> u64 {
        match self {
            OverrideAddr::Address(_) => 0,
            OverrideAddr::Range(a, b) => b - a + 1,
        }
    }
}

impl Display for OverrideAddr {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            OverrideAddr::Address(addr) => {
                write!(f, "${addr:06X}")
            }
            OverrideAddr::Range(range_begin, range_end) => {
                write!(f, "${range_begin:06X}..${range_end:06X}")
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Deserialize)]
pub enum OperandType {
    Literal,
    Address,
    /// Address which will be labelled even if it's value is 0
    NonNullAddress,
}

#[derive(PartialEq, Deserialize)]
pub struct Override {
    pub addr: OverrideAddr,

    /// Label at this address will be used as the label (with an offset if necessary)
    pub label_addr: Option<Addr>,
    /// Specifies a bank which will be used to lookup short addresses. Otherwise falls back to a
    /// logged bank, or the same as a program bank. If set on an immediate, then it'll be assumed
    /// to be a short pointer to this bank when looking up a label.
    pub db: Option<Bank>,

    /// Determines if the operand value will be emitted as a numeric literal, or as a reference to
    /// an appropriate label. If `db` or `label_addr` are set, this defaults to [`Address`],
    /// otherwise:
    /// - For code locations, immediates default to [`Literal`] and memory references default to
    ///   [`Address`].
    /// - For data locations, `dl` defaults to [`Address`], otherwise defaults to [`Literal`].
    ///
    /// [`Address`]: OperandType::Address
    /// [`Literal`]: OperandType::Literal
    #[serde(rename = "type")]
    pub operand_type: Option<OperandType>,

    /// Labels auto-generated from this operand will use this as the label name prefix.
    pub label_name_hint: Option<String>,

    #[serde(rename = "struct")]
    pub struct_: Option<String>,
}

impl Override {
    pub fn new(addr: OverrideAddr) -> Self {
        Self {
            addr,
            label_addr: None,
            db: None,
            operand_type: None,
            label_name_hint: None,
            struct_: None,
        }
    }
}

impl Debug for Override {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let &Self {
            addr,
            label_addr,
            db,
            operand_type,
            label_name_hint,
            struct_,
        } = &self;

        macro_rules! fmt_field {
            ($field:ident) => {
                if let Some($field) = $field {
                    write!(f, concat!(stringify!($field), ": {:#X?}, "), $field)?;
                }
            };
        }

        write!(f, "Override {{ addr: {addr}, ")?;
        fmt_field!(label_addr);
        fmt_field!(db);
        fmt_field!(operand_type);
        fmt_field!(label_name_hint);
        fmt_field!(struct_);
        write!(f, "}}")
    }
}

#[derive(Debug, PartialEq)]
pub struct Config {
    pub labels: Vec<Label>,
    overrides: Vec<Override>,
    pub structs: Vec<Struct>,
}

impl Config {
    pub fn load(path: &str) -> Result<Config> {
        fn read_config_entries<T: for<'a> Deserialize<'a>>(files_pattern: &str) -> Result<Vec<T>> {
            let filenames = glob(files_pattern).unwrap();
            filenames
                .map(|path| {
                    let path = path?;
                    let file = File::open(&path)?;
                    let contents: Vec<T> = serde_yaml::from_reader(file)
                        .with_context(|| format!("Failed deserializing {:?}", &path))?;
                    Ok(contents)
                })
                .flatten_ok()
                .collect()
        }

        let labels: Vec<Label> = read_config_entries(&format!("{path}/labels/*.yaml"))?;
        let mut overrides: Vec<Override> =
            read_config_entries(&format!("{path}/overrides/*.yaml"))?;
        let structs: Vec<Struct> = read_config_entries(&format!("{path}/structs/*.yaml"))?;

        /* Generate overrides from pointer labels with a length defined */
        // TODO: This is probably nicer if done after config generation together with the struct override generation
        let generated_overrides = labels.iter().filter_map(|l| {
            let override_type = match &l.type_ {
                Some(LabelType::Data | LabelType::DataTable) if l.addr.length() > 0 => {
                    OperandType::Literal
                }
                Some(LabelType::Pointer | LabelType::PointerTable) if l.addr.length() > 0 => {
                    OperandType::Address
                }
                // Struct auto-overrides are handled in structs::generate_overrides
                _ => return None,
            };

            Some(Override {
                db: Some((l.addr.first() >> 16) as Bank),
                operand_type: Some(override_type),
                // Address ranges are inclusive
                ..Override::new(l.addr)
            })
        });
        overrides.extend(generated_overrides);

        Ok(Config {
            labels,
            overrides,
            structs,
        })
    }

    pub fn get_override(&self, addr: Addr) -> Option<&Override> {
        self.overrides.iter().find(|o| match &o.addr {
            OverrideAddr::Address(a) if *a == addr => true,
            OverrideAddr::Range(a, b) if addr >= *a && addr <= *b => true,
            _ => false,
        })
    }

    pub fn add_overrides(&mut self, override_iter: impl IntoIterator<Item = Override>) {
        // TODO: Love me some O(n^2) (use a BTreeMap)
        'outer: for override_ in override_iter {
            for o in &self.overrides {
                if override_.addr.overlaps(&o.addr) {
                    eprintln!("Overlapping overrides: {override_:?} and {o:?}");
                    continue 'outer;
                }
            }
            self.overrides.push(override_);
        }
    }

    pub fn get_struct(&self, name: &str) -> Option<&Struct> {
        self.structs.iter().find(|s| name == s.name)
    }
}
