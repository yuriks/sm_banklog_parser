#![warn(clippy::pedantic)]
#![allow(clippy::similar_names)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::no_effect_underscore_binding)]
#![allow(clippy::verbose_bit_mask)]
#![allow(clippy::items_after_statements)]

use anyhow::{anyhow, Context};
use std::io::BufWriter;
use std::{
    collections::BTreeMap,
    fs::File,
    io::{BufRead, BufReader, Write},
    mem,
};

use glob::glob;
use regex::Regex;

use code::Code;

use crate::config::Config;
use crate::data::Data;
use crate::directives::InstructionPrototype;
use crate::label::{Label, LabelMap, LabelName, LabelType};
use crate::line::{Line, LineContent};
use crate::opcode::StaticAddress;
use crate::operand::{Override, OverrideAddr, OverrideMap};

mod code;
mod config;
mod data;
mod directives;
mod label;
mod line;
mod opcode;
mod operand;
mod parse;
mod structs;

pub type Addr = u64;
pub type Bank = u8;

pub(crate) fn addr_with_bank(bank: Bank, addr: Addr) -> Addr {
    (Addr::from(bank) << 16) + (addr & 0xFFFF)
}

pub(crate) fn addr16_with_bank(bank: Bank, addr: u16) -> Addr {
    (Addr::from(bank) << 16) + Addr::from(addr)
}

pub(crate) fn split_addr(addr: Addr) -> (Bank, Addr) {
    ((addr >> 16) as Bank, addr & 0xFFFF)
}

pub(crate) fn split_addr16(addr: Addr) -> (Bank, u16) {
    ((addr >> 16) as Bank, (addr & 0xFFFF) as u16)
}

fn is_bulk_data(addr: u32) -> bool {
    #[allow(clippy::match_same_arms)]
    match addr {
        0x87_8564..=0x87_FFFF => true, // Animated tiles
        0x89_8000..=0x89_90FF => true, // Item PLM graphics
        0x8A_8000..=0x8A_E97F => true, // FX layer 3 tilemaps
        0x8E_8000..=0x8E_E5FF => true, // Menu tiles
        0x94_C800..=0x99_FFFF => true, // Cutscene graphics
        0x9A_8200..=0x9A_FBFF => true, // Projectile & map graphics
        0x9B_8000..=0x9B_93FF => true, // Tiles - Samus death sequence
        0x9B_E000..=0x9F_FFFF => true, // Samus tiles
        0xAB_8000..=0xAD_DDFF => true, // Various enemy tiles
        0xAE_8000..=0xB1_FFFF => true, // More enemy tiles
        0xB5_8000..=0xB5_FFFF => true, // Region maps
        0xB6_8000..=0xB6_EFFF => true, // Pause screen tiles/tilemaps
        0xB7_8000..=0xB7_FFFF => true, // More enemy graphics
        0xB9_8000..=0xDF_FFFF => true, // Compressed data & music
        _ => false,
    }
}

#[derive(Copy, Clone, Debug)]
pub enum SpecialParsingType {
    Spritemap,
    SpritemapRaw,
    SpritemapExtended,
    InstructionList,
}

#[derive(Clone, Default)]
struct ParsingModifiers {
    data_type: Option<SpecialParsingType>,
}

impl ParsingModifiers {
    fn set_data_type(&mut self, new_type: SpecialParsingType) -> Result<(), String> {
        match &self.data_type {
            None => {
                self.data_type = Some(new_type);
                Ok(())
            }
            Some(current_type) => Err(format!("Special data type already set: {current_type:?}")),
        }
    }
}

pub struct FileParsingState {
    modifier_stack: Vec<ParsingModifiers>,
    prefixed_instruction_directive: Option<InstructionPrototype>,

    last_data_cmd: String,
    cur_addr: Addr,
}

impl FileParsingState {
    fn new(start_addr: u64) -> Self {
        FileParsingState {
            modifier_stack: vec![ParsingModifiers::default()],
            prefixed_instruction_directive: None,
            last_data_cmd: String::new(),
            cur_addr: start_addr,
        }
    }

    fn get_modifiers(&self) -> &ParsingModifiers {
        self.modifier_stack.last().unwrap()
    }

    fn get_modifiers_mut(&mut self) -> &mut ParsingModifiers {
        self.modifier_stack.last_mut().unwrap()
    }

    fn pop_context(&mut self) -> Result<(), String> {
        if self.modifier_stack.len() > 1 {
            self.modifier_stack.pop().unwrap();
            Ok(())
        } else {
            Err("tried to pop root context".into())
        }
    }

    fn push_context(&mut self) {
        let current_context = self.get_modifiers().clone();
        self.modifier_stack.push(current_context);
    }

    fn addr_in_current_bank(&self, word_addr: u16) -> Addr {
        (self.cur_addr & !0xFFFF) + Addr::from(word_addr)
    }
}

type Banks = BTreeMap<Bank, Vec<Line>>;

fn parse_files(labels: &mut LabelMap) -> Banks {
    let mut result = BTreeMap::new();

    let filenames = glob("./logs/*.asm").unwrap();
    let filename_regex = Regex::new(r"Bank \$([0-9A-F]{2})(?:\.\.\$([0-9A-F]{2}))?").unwrap();
    for filename in filenames.flatten() {
        let cap = filename_regex.captures(filename.to_str().unwrap()).unwrap();
        let bank_start = u8::from_str_radix(&cap[1], 16).unwrap();
        let bank_end = cap
            .get(2)
            .map_or(bank_start, |s| u8::from_str_radix(s.as_str(), 16).unwrap());

        let file = BufReader::new(File::open(&filename).unwrap());
        let mut file_state = FileParsingState::new(addr16_with_bank(bank_start, 0x8000));
        let mut lines = Vec::new();
        let mut elide_bulk_data = false;

        /* Parse the full file into data */
        for line in file.lines() {
            let line = line.unwrap(); // TODO: Error handling
            let parsed = Line::parse(&line, &mut file_state);

            if let Some(address) = parsed.address() {
                let (bank, _) = split_addr16(address);
                if (bank < bank_start || bank > bank_end) && parsed.contents.produces_output() {
                    eprintln!(
                        "Line defined outside of the bank range of its file: ${:06X} in `{}`. Line: {:#X?}",
                        address,
                        filename.display(),
                        parsed
                    );
                }
            }

            if let LineContent::Code(Code {
                address,
                instruction_prototype: Some(instr_proto),
                ..
            }) = &parsed.contents
            {
                let new_label = Label::new(
                    *address,
                    LabelName::AutoGenerated(format!("SUB_{address:06X}")),
                    LabelType::Instruction(instr_proto.clone()),
                    0,
                );
                if let Err((_, l)) = labels.insert_label(new_label) {
                    eprintln!("Duplicate prototype for label {}", l.name());
                }
            }

            match parsed.contents {
                LineContent::Data(Data { address, .. }) if is_bulk_data(address as u32) => {
                    // TODO: Also elide the ascii art comments
                    if !elide_bulk_data {
                        lines.push(Line::new_comment(" [pjdasm] Bulk data omitted"));
                        elide_bulk_data = true;
                    }
                }
                _ => {
                    lines.push(parsed);
                    elide_bulk_data = false;
                }
            }
        }

        result.insert(bank_start, lines);
    }

    result
}

/// Duplicates the code at the start of bank $A0 to the other enemy banks where its present. This
/// repeated section is elided in the bank logs.
fn clone_shared_enemy_ai_library(
    banks: &mut Banks,
    overrides: &mut OverrideMap,
    labels: &mut LabelMap,
) -> anyhow::Result<()> {
    // Gather source lines from bank $A0
    let template_lines: Vec<Line> = {
        let a0_lines = banks.get(&0xA0).context("Bank $A0 not in input")?;
        let mut it = a0_lines.iter();

        // Advances iterator until the header line we want
        it.find(|l| {
            l.comment
                .as_ref()
                .is_some_and(|c| c.starts_with(";; $8000..8686: Common to all enemy banks"))
        })
        .context("Reference line not found")?;

        let is_open_bracket = |l: &&Line| matches!(&l.contents, LineContent::Bracket('{'));
        let is_close_bracket = |l: &&Line| matches!(&l.contents, LineContent::Bracket('}'));

        it.next()
            .filter(is_open_bracket)
            .context("Reference line not followed by {")?;

        let mut nesting_level = 1usize;
        let result = it
            .take_while(|l| {
                if is_open_bracket(l) {
                    nesting_level += 1;
                } else if is_close_bracket(l) {
                    nesting_level -= 1;
                }

                nesting_level != 0
            })
            .cloned()
            .collect();

        if nesting_level != 0 {
            return Err(anyhow!("Unclosed bracket"));
        }

        result
    };

    // Copy enemy banks to respective new bank
    let enemy_banks = (0xA2u8..=0xAA).chain(0xB2..=0xB3);
    for bank in enemy_banks {
        let bank_lines = banks
            .get_mut(&bank)
            .with_context(|| format!("Bank ${bank:02X} doesn't exist"))?;

        let insertion_pos = 1 + bank_lines
            .iter()
            .position(|l| {
                l.comment
                    .as_ref()
                    .is_some_and(|c| c.trim_start().starts_with("See bank $A0"))
            })
            .with_context(|| format!("Target line not found in bank ${bank:02X}"))?;

        bank_lines.splice(
            insertion_pos..insertion_pos,
            template_lines.iter().cloned().map(|line| {
                let Some(address) = line.address() else {
                    return line;
                };

                let new_addr = addr_with_bank(bank, address);
                if let LineContent::Code(c) = &line.contents {
                    if let Some(instr_proto) = &c.instruction_prototype {
                        let new_label = Label::new(
                            new_addr,
                            LabelName::AutoGenerated(format!("SUB_{new_addr:06X}")),
                            LabelType::Instruction(instr_proto.clone()),
                            0,
                        );
                        if let Err((_, l)) = labels.insert_label(new_label) {
                            eprintln!("Duplicate prototype for label {}", l.name());
                        }
                    }

                    // Auto-add overrides to fix labels that referenced bank $A0
                    if let StaticAddress::DataBank(_low_addr) = c.get_operand() {
                        if let Some(0xA0) =
                            c.get_operand_label_address(None).map(|a| split_addr16(a).0)
                        {
                            overrides.add_override(Override {
                                db: Some(0xA0),
                                ..Override::new(OverrideAddr::Address(new_addr))
                            });
                        }
                    }
                }

                line.with_address(new_addr)
            }),
        );
    }

    Ok(())
}

fn write_output_files(banks: &Banks, overrides: &OverrideMap, labels: &LabelMap) {
    fn create_output_file(path: &str) -> BufWriter<File> {
        BufWriter::new(File::create(format!("./asm/{path}")).unwrap())
    }

    let mut main_file = create_output_file("main.asm");
    writeln!(main_file, "lorom").unwrap();
    writeln!(main_file, "incsrc labels.asm").unwrap();

    for (bank, bank_lines) in banks {
        let mut output_file = create_output_file(&format!("bank_{bank:02x}.asm"));
        writeln!(main_file, "incsrc bank_{bank:02x}.asm").unwrap();

        let mut current_addr = Addr::MAX;

        for line in bank_lines {
            if let Some(address) = line.address() {
                if address != current_addr {
                    current_addr = address;
                    writeln!(output_file, "org ${current_addr:06X}").unwrap();
                }

                let pc_advance = line.contents.pc_advance();
                if pc_advance != 0 {
                    let label = labels.get_label_exact(address);
                    if let Some(label) = label {
                        writeln!(output_file, "{}:", label.name()).unwrap();
                        label.assigned.set(true);
                    }
                    current_addr += pc_advance;
                }
            }

            writeln!(output_file, "{}", line.to_string(overrides, labels)).unwrap();
        }
    }

    fn dump_labels(labels: &LabelMap, filename: &str, pred: impl FnMut(&&Label) -> bool) {
        let mut output_file = create_output_file(filename);
        writeln!(output_file, "; Generated by pjdasm").unwrap();
        for l in labels.iter_labels().filter(pred) {
            writeln!(output_file, "{} = ${:06X}", l.name(), l.address).unwrap();
        }
    }

    dump_labels(labels, "labels.asm", |l| {
        !l.assigned.get() && !l.is_blocked()
    });
    dump_labels(labels, "all_labels.asm", |l| {
        l.assigned.get() && !l.is_blocked()
    });
    dump_labels(labels, "externals.asm", |l| {
        l.assigned.get() && l.is_external() && !l.is_blocked()
    });
}

fn main() {
    let mut config = Config::load("./config/").expect("Failed to read config");
    let mut overrides = OverrideMap::from_config(mem::take(&mut config.overrides));
    let mut labels = LabelMap::from_config(mem::take(&mut config.labels));

    println!("Parsing...");
    let mut banks = parse_files(&mut labels);

    println!("Indexing...");
    clone_shared_enemy_ai_library(&mut banks, &mut overrides, &mut labels).unwrap();
    label::generate_overrides(&mut overrides, &labels);
    structs::generate_overrides(&mut overrides, &config, &labels);
    label::generate_labels(&mut labels, &banks, &overrides);

    println!("Generating...");
    write_output_files(&banks, &overrides, &labels);
}
