use std::fmt::{Display, Formatter};
use glob::glob;
use serde::Deserialize;
use crate::Bank;

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
    pub db: Option<u64>
}

#[derive(Debug, PartialEq, Deserialize)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>
}

#[derive(Debug, PartialEq, Deserialize)]
pub struct Label {
    pub addr: u64,
    pub name: String,
    // Needs to be an Option because default doesn't work: https://github.com/serde-rs/serde/issues/1626
    #[serde(rename = "type", flatten, default)]
    pub type_: Option<LabelType>,
}

#[derive(Debug, PartialEq, Deserialize)]
#[serde(untagged)]
pub enum OverrideAddr {
    Address(u64),
    Range(u64, u64)
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
    pub db: Option<Bank>,
    #[serde(rename = "type")]
    pub type_: Option<OverrideType>,
    #[serde(rename = "struct")]
    pub struct_: Option<String>,
    pub opcode: Option<Vec<u64>>,
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
                    db: Some((l.addr >> 16) as Bank),
                    struct_: None,
                    type_: Some(override_type),
                    opcode: None
                })
            });
        overrides.extend(generated_overrides);

        Config { labels, overrides, structs }
    }

    pub fn get_override(&self, addr: u64) -> Option<&Override> {
        self.overrides.iter().find(|o| match &o.addr {
            OverrideAddr::Address(a) if *a == addr => true,
            OverrideAddr::Range(a, b) if addr >= *a && addr <= *b => true,
            _ => false
        })
    }
}