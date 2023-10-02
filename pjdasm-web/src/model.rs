use std::mem;

use arc_swap::ArcSwap;
use pjdasm::config::Config;
use pjdasm::label::LabelMap;
use pjdasm::operand::OverrideMap;
use pjdasm::output_tokens::{Token, TokenString, TokenWriter};
use pjdasm::{label, structs, Addr, Bank, Banks};
use serde::Serialize;

use crate::api::{Error, Result};

pub struct AppState {
    pub persistent: PersistentData,
    pub processed: ArcSwap<ProcessedData>,
}

impl AppState {
    pub fn new() -> AppState {
        let persistent = load_initial();
        let processed = refresh(&persistent);
        AppState {
            persistent,
            processed: ArcSwap::from_pointee(processed),
        }
    }
}

pub struct PersistentData {
    config: Config,
    overrides: OverrideMap,
    labels: LabelMap,
    banks: Banks,
}

pub fn load_initial() -> PersistentData {
    let mut config = Config::load("./config/").expect("Failed to read config");
    let mut overrides = OverrideMap::from_config(mem::take(&mut config.overrides));
    let mut labels = LabelMap::from_config(mem::take(&mut config.labels));

    println!("Parsing...");
    let mut banks = pjdasm::parse_files();

    pjdasm::generate_instruction_prototype_labels(&banks, &mut labels);
    pjdasm::clone_shared_enemy_ai_library(&mut banks, &mut overrides, &mut labels).unwrap();

    PersistentData {
        config,
        overrides,
        labels,
        banks,
    }
}

pub struct ProcessedData {
    overrides: OverrideMap,
    labels: LabelMap,
}

fn refresh(persistent: &PersistentData) -> ProcessedData {
    let PersistentData {
        config,
        overrides,
        labels,
        banks,
    } = persistent;

    let mut overrides = (*overrides).clone();
    let mut labels = (*labels).clone();

    println!("Indexing...");
    label::generate_overrides(&mut overrides, &labels);
    structs::generate_overrides(&mut overrides, &config, &labels);
    label::generate_labels(&mut labels, &banks, &overrides);

    ProcessedData { overrides, labels }
}

// Address + sub-line
#[derive(Serialize)]
struct LineKey(u32, u32);

#[derive(Serialize)]
pub struct DisplayLine {
    key: LineKey,
    address: Option<Addr>,
    prefix_lines: String,
    content: String,
    tokens: Vec<Token>,
}

pub fn get_bank_for_display(
    bank: Bank,
    PersistentData { banks, .. }: &PersistentData,
    ProcessedData { overrides, labels }: &ProcessedData,
) -> Result<Vec<DisplayLine>> {
    let lines = banks.get(&bank).ok_or(Error::BankNotFound { bank })?;

    let mut result = Vec::new();

    pjdasm::emit_bank_lines(lines, overrides, labels, |idx, disp, _line| {
        let mut content = TokenWriter::new();
        pjdasm::format_line(&mut content, &disp, false)?;
        let content: TokenString = content.into();

        result.push(DisplayLine {
            key: LineKey(idx.try_into()?, 0),
            address: disp.address,
            prefix_lines: disp.prefix_lines,
            content: content.text,
            tokens: content.tokens,
        });
        Ok(())
    })?;

    Ok(result)
}
