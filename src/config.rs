use std::fmt::Debug;
use std::fs::File;
use std::io::BufReader;

use anyhow::{Context, Result};
use glob::glob;
use itertools::Itertools;
use serde::Deserialize;

use crate::label::LabelType;
use crate::operand::{Override, OverrideAddr};
use crate::{Addr, Bank};

#[derive(Debug, PartialEq, Deserialize)]
pub struct StructField {
    pub name: String,
    pub offset: Addr,
    pub length: Addr,
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
    pub fn size(&self) -> Addr {
        let Some(last_field) = self.fields.last() else {
            return 0;
        };
        last_field.offset + last_field.length
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

#[derive(Debug, PartialEq)]
pub struct Config {
    pub labels: Vec<Label>,
    pub overrides: Vec<Override>,
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

        Ok(Config {
            labels: read_config_entries(&format!("{path}/labels/*.yaml"))?,
            overrides: read_config_entries(&format!("{path}/overrides/*.yaml"))?,
            structs: read_config_entries(&format!("{path}/structs/*.yaml"))?,
        })
    }

    pub fn get_struct(&self, name: &str) -> Option<&Struct> {
        self.structs.iter().find(|s| name == s.name)
    }
}
