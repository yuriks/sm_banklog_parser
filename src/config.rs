use std::fmt::{Display, Formatter};

use glob::glob;
use serde::Deserialize;

use crate::{Addr, Bank};
use crate::label::LabelType;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Deserialize)]
pub enum OverrideType {
    Pointer,
    Data,
    Struct,
    PointerTable,
    DataTable,
    Subroutine,
}

#[derive(Debug, PartialEq, Deserialize)]
pub struct StructField {
    pub name: String,
    pub offset: u64,
    pub length: u64,
    #[serde(rename = "type")]
    pub type_: OverrideType,
    pub db: Option<Bank>
}

#[derive(Debug, PartialEq, Deserialize)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>
}

impl Struct {
    pub fn size(&self) -> u64 {
        let Some(last_field) = self.fields.last() else { return 0; };
        last_field.offset + last_field.length
    }

    pub fn field_at_offset(&self, offset: u64) -> Option<&StructField> {
        self.fields.iter().find(|f| f.offset == offset)
    }
}

#[derive(Debug, PartialEq, Deserialize)]
pub struct Label {
    pub addr: Addr,
    pub name: String,
    // Needs to be an Option because default doesn't work: https://github.com/serde-rs/serde/issues/1626
    #[serde(rename = "type", flatten, default)]
    pub type_: Option<LabelType>,
}

#[derive(Debug, PartialEq, Deserialize)]
#[serde(untagged)]
pub enum OverrideAddr {
    Address(Addr),
    Range(Addr, Addr)
}

impl Display for OverrideAddr {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            OverrideAddr::Address(addr) => {
                write!(f, "${addr:06X}")
            },
            OverrideAddr::Range(range_begin, range_end) => {
                write!(f, "${range_begin:06X}..${range_end:06X}")
            },
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Deserialize)]
pub enum OperandType {
    Literal,
    Address,
}

#[derive(Debug, PartialEq, Deserialize)]
pub struct Override {
    pub addr: OverrideAddr,

    /// Label at this address will be used as the label (with an offset if necessary)
    pub label_addr: Option<Addr>,
    /// Specifies a bank which will be used to lookup short addresses. Otherwise falls back to a
    /// logged bank, or the same as a program bank. If set on an immediate, then it'll be assumed
    /// to be a short pointer to this bank when looking up a label.
    pub db: Option<Bank>,

    // TODO: Remove and start using `operand_type` instead
    #[serde(rename = "type")]
    pub type_: Option<OverrideType>,

    /// Determines if the operand value will be emitted as a numeric literal, or as a reference to
    /// an appropriate label. If `db` or `label_addr` are set, this defaults to [`Address`],
    /// otherwise:
    /// - For code locations, immediates default to [`Literal`] and memory references default to
    ///   [`Address`].
    /// - For data locations, `dl` defaults to [`Address`], otherwise defaults to [`Literal`].
    ///
    /// [`Address`]: OperandType::Address
    /// [`Literal`]: OperandType::Literal
    pub operand_type: Option<OperandType>,

    #[serde(rename = "struct")]
    pub struct_: Option<String>,
}

impl Override {
    pub fn new(addr: OverrideAddr) -> Self {
        Self {
            addr,
            label_addr: None,
            db: None,
            type_: None,
            operand_type: None,
            struct_: None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Config {
    pub labels: Vec<Label>,
    pub overrides: Vec<Override>,
    pub structs: Vec<Struct>
}

impl Config {
    pub fn load(path: &str) -> Config {
        let label_filenames = glob(&format!("{path}/labels/*.yaml")).unwrap();
        let labels: Vec<Label> = label_filenames.flatten()
            .flat_map(|f| serde_yaml::from_str::<Vec<Label>>(&std::fs::read_to_string(&f).unwrap()).unwrap_or_else(|e| panic!("Failed parsing {f:?}: {e}")))
            .collect();

        let override_filenames = glob(&format!("{path}/overrides/*.yaml")).unwrap();
        let mut overrides: Vec<Override> = override_filenames.flatten()
            .flat_map(|f| serde_yaml::from_str::<Vec<Override>>(&std::fs::read_to_string(f).unwrap()).unwrap())
            .collect();

        let struct_filenames = glob(&format!("{path}/structs/*.yaml")).unwrap();
        let structs: Vec<Struct> = struct_filenames.flatten()
            .flat_map(|f| serde_yaml::from_str::<Vec<Struct>>(&std::fs::read_to_string(f).unwrap()).unwrap())
            .collect();

        /* Generate overrides from pointer labels with a length defined */
        let generated_overrides = labels.iter()
            .filter_map(|l| {
                let (override_type, length) = match l.type_ {
                    Some(LabelType::PointerTable { length }) if length > 1 => (OverrideType::Pointer, length),
                    Some(LabelType::DataTable { length }) if length > 1 => (OverrideType::Data, length),
                    _ => return None,
                };

                Some(Override {
                    db: Some((l.addr >> 16) as Bank),
                    type_: Some(override_type),
                    // Address ranges are inclusive
                    ..Override::new(OverrideAddr::Range(l.addr, l.addr + (length * 2) - 1))
                })
            });
        overrides.extend(generated_overrides);

        Config { labels, overrides, structs }
    }

    pub fn get_override(&self, addr: Addr) -> Option<&Override> {
        self.overrides.iter().find(|o| match &o.addr {
            OverrideAddr::Address(a) if *a == addr => true,
            OverrideAddr::Range(a, b) if addr >= *a && addr <= *b => true,
            _ => false
        })
    }
}