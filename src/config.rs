use std::collections::BTreeMap;
use std::fmt::{Debug, Display, Formatter};
use std::fs::File;
use std::io::BufReader;

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

    fn overlaps(&self, o: &OverrideAddr) -> bool {
        let (start1, end1) = self.as_range();
        let (start2, end2) = o.as_range();

        start1 <= end2 && end1 >= start2
    }

    pub fn first(self) -> Addr {
        self.as_range().0
    }

    fn last(self) -> Addr {
        self.as_range().1
    }

    pub fn contains(self, addr: Addr) -> bool {
        match self {
            OverrideAddr::Address(x) => addr == x,
            OverrideAddr::Range(a, b) => addr >= a && addr <= b,
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
    overrides: BTreeMap<Addr, Override>,
    pub structs: Vec<Struct>,
}

impl Config {
    pub fn load(path: &str) -> Result<Config> {
        fn read_config_entries<T: for<'a> Deserialize<'a>>(files_pattern: &str) -> Result<Vec<T>> {
            let filenames = glob(files_pattern).unwrap();
            filenames
                .map(|path| {
                    let path = path?;
                    let file = BufReader::new(File::open(&path)?);
                    let contents: Vec<T> = serde_yaml::from_reader(file)
                        .with_context(|| format!("Failed deserializing {:?}", &path))?;
                    Ok(contents)
                })
                .flatten_ok()
                .collect()
        }

        let labels: Vec<Label> = read_config_entries(&format!("{path}/labels/*.yaml"))?;
        let overrides = BTreeMap::from_iter(
            read_config_entries::<Override>(&format!("{path}/overrides/*.yaml"))?
                .into_iter()
                .map(|o| (o.addr.first(), o)),
        );
        let structs: Vec<Struct> = read_config_entries(&format!("{path}/structs/*.yaml"))?;

        Ok(Config {
            labels,
            overrides,
            structs,
        })
    }

    pub fn get_override(&self, addr: Addr) -> Option<&Override> {
        let (_, ov) = self.overrides.range(..=addr).last()?;
        if ov.addr.contains(addr) {
            Some(ov)
        } else {
            None
        }
    }

    pub fn add_override(&mut self, override_: Override) {
        // Check for an overlapping override by checking insertion neighbors
        let preceding_ov = self
            .overrides
            .range(..=override_.addr.first())
            .last()
            .map(|(_, v)| v);
        let following_ov = self
            .overrides
            .range(override_.addr.last()..)
            .next()
            .map(|(_, v)| v);
        for neighbor in itertools::chain(preceding_ov, following_ov) {
            if override_.addr.overlaps(&neighbor.addr) {
                eprintln!("Overlapping overrides: {override_:?} and {neighbor:?}");
                return;
            }
        }

        self.overrides.insert(override_.addr.first(), override_);
    }

    pub fn get_struct(&self, name: &str) -> Option<&Struct> {
        self.structs.iter().find(|s| name == s.name)
    }
}
