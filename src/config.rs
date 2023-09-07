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

#[derive(Debug, PartialEq, Deserialize)]
pub struct Override {
    pub addr: OverrideAddr,
    /// Label at this address will be used as the label (with an offset if necessary)
    pub label_addr: Option<Addr>,
    /// Specifies a bank which will be used to lookup short addresses. Otherwise falls back to a
    /// logged bank, or the same as a program bank. If set on an immediate, then it'll be assumed
    /// to be a short pointer to this bank when looking up a label.
    pub db: Option<Bank>,
    #[serde(rename = "type")]
    pub type_: Option<OverrideType>,
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
                    // Address ranges are inclusive
                    addr: OverrideAddr::Range(l.addr, l.addr + (length * 2) - 1),
                    label_addr: None,
                    db: Some((l.addr >> 16) as Bank),
                    struct_: None,
                    type_: Some(override_type),
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