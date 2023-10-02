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

use std::io::BufWriter;
use std::{
    collections::BTreeMap,
    fs::File,
    io::{BufRead, BufReader, Write},
};

use glob::glob;
use itertools::Itertools;
use regex::Regex;

use code::Code;
use data::Data;
use line::Line;

use crate::config::{Config, Override, OverrideAddr};
use crate::directives::InstructionPrototype;
use crate::label::{Label, LabelMap, LabelName, LabelType};
use crate::opcode::StaticAddress;

mod code;
mod config;
mod data;
mod directives;
mod label;
mod line;
mod opcode;
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
    // TODO: This might be redundant with cur_addr
    last_pc: u64,
    cur_addr: u64,
}

impl FileParsingState {
    fn new(start_addr: u64) -> Self {
        FileParsingState {
            modifier_stack: vec![ParsingModifiers::default()],
            prefixed_instruction_directive: None,
            last_data_cmd: String::new(),
            last_pc: 0,
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

fn parse_files(lines: &mut BTreeMap<Addr, Vec<Line>>, labels: &mut LabelMap) {
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

        /* Parse the full file into data */
        for line in file.lines() {
            let line = line.unwrap(); // TODO: Error handling
            let (parsed_addr, parsed_line) = Line::parse(&line, &mut file_state);
            if let Some(addr) = parsed_addr {
                let (bank, _) = split_addr16(addr);
                if bank < bank_start || bank > bank_end {
                    eprintln!(
                        "Line defined outside of the bank range of its file: ${addr:06X} in `{}`",
                        filename.display()
                    );
                }
                file_state.cur_addr = addr;
            }

            if let Line::Code(Code {
                instruction_prototype: Some(instr_proto),
                ..
            }) = &parsed_line
            {
                let new_label = Label::new(
                    file_state.cur_addr,
                    LabelName::AutoGenerated(format!("SUB_{:06X}", file_state.cur_addr)),
                    LabelType::Instruction(instr_proto.clone()),
                    0,
                );
                if let Err((_, l)) = labels.insert_label(new_label) {
                    eprintln!("Duplicate prototype for label {}", l.name());
                }
            }

            if !is_bulk_data(file_state.cur_addr as u32) {
                lines
                    .entry(file_state.cur_addr)
                    .or_default()
                    .push(parsed_line);
            }
        }
    }
}

/// Duplicates the code at the start of bank $A0 to the other enemy banks where its present. This
/// repeated section is elided in the bank logs.
fn clone_shared_enemy_ai_library(
    lines: &mut BTreeMap<Addr, Vec<Line>>,
    config: &mut Config,
    labels: &mut LabelMap,
) {
    // Can't mutate lines while iterating through it, so accumulate results in temporary Vec
    let mut new_lines = Vec::new();

    // Copy enemy banks to respective new bank
    for (&addr, addr_lines) in lines.range(0xA0_8000..=0xA0_8686) {
        let enemy_banks = (0xA2u8..=0xAA).chain(0xB2..=0xB3);
        for bank in enemy_banks {
            let mut new_addr_lines = Vec::new();
            let new_addr = addr_with_bank(bank, addr);

            for line in addr_lines {
                let new_line = match line {
                    Line::Code(c) => {
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
                                config.add_override(Override {
                                    db: Some(0xA0),
                                    ..Override::new(OverrideAddr::Address(new_addr))
                                });
                            }
                        }

                        Line::Code(Code {
                            address: addr_with_bank(bank, c.address),
                            ..c.clone()
                        })
                    }
                    Line::Data(d) => Line::Data(Data {
                        address: addr_with_bank(bank, d.address),
                        ..d.clone()
                    }),
                    Line::Comment(c) => Line::Comment(c.clone()),
                };

                new_addr_lines.push(new_line);
            }

            new_lines.push((new_addr, new_addr_lines));
        }
    }
    lines.extend(new_lines);
}

fn write_output_files(lines: &BTreeMap<Addr, Vec<Line>>, config: &Config, labels: &LabelMap) {
    fn create_output_file(path: &str) -> BufWriter<File> {
        BufWriter::new(File::create(format!("./asm/{path}")).unwrap())
    }

    let mut main_file = create_output_file("main.asm");
    writeln!(main_file, "lorom").unwrap();
    writeln!(main_file, "incsrc labels.asm").unwrap();

    for (bank, bank_addrs) in &lines.iter().group_by(|(&addr, _)| split_addr16(addr).0) {
        let mut output_file = create_output_file(&format!("bank_{bank:02x}.asm"));
        writeln!(main_file, "incsrc bank_{bank:02x}.asm").unwrap();

        let mut current_addr = Addr::MAX;

        for (&addr, addr_lines) in bank_addrs {
            if addr != current_addr {
                current_addr = addr;
                writeln!(output_file, "org ${current_addr:06X}").unwrap();
            }

            if let Some(label) = labels.get_label_exact(addr) {
                writeln!(
                    output_file,
                    "{}{}",
                    label.name(),
                    if label.name().starts_with('.') {
                        ""
                    } else {
                        ":"
                    }
                )
                .unwrap();
                label.assigned.set(true);
            }

            for line in addr_lines {
                writeln!(output_file, "{}", line.to_string(config, labels)).unwrap();
                current_addr += line.pc_advance();
            }
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

    println!("Parsing...");
    let mut lines = BTreeMap::new();
    let mut labels = LabelMap::new();
    parse_files(&mut lines, &mut labels);

    println!("Indexing...");

    clone_shared_enemy_ai_library(&mut lines, &mut config, &mut labels);

    /* Autogenerate labels */
    label::load_labels(&config, &mut labels);
    label::generate_overrides(&mut config, &labels);
    structs::generate_overrides(&mut config, &labels);
    label::generate_labels(&lines, &config, &mut labels);

    println!("Generating...");
    write_output_files(&lines, &config, &labels);
}
