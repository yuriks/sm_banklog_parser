#![warn(clippy::pedantic)]
#![allow(clippy::similar_names)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::no_effect_underscore_binding)]
#![allow(clippy::verbose_bit_mask)]

use std::collections::btree_map::Entry;
use std::{
    collections::BTreeMap,
    fs::File,
    io::{BufRead, BufReader, Write},
};

use glob::glob;
use regex::Regex;

use code::Code;
use data::Data;
use line::Line;

use crate::config::{Override, OverrideAddr};
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

struct GlobalParsingState {
    pub labels: LabelMap,
}

impl GlobalParsingState {
    fn new() -> Self {
        GlobalParsingState {
            labels: LabelMap::new(),
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

fn main() {
    let filename_regex = Regex::new(r"Bank \$([0-9A-F]{2})(\.\.\$([0-9A-F]{2})|)").unwrap();
    let mut bank_groups: Vec<(u8, u8)> = Vec::new();
    let mut config = config::Config::load("./config/").expect("Failed to read config");

    let mut global_state = GlobalParsingState::new();
    let mut lines: BTreeMap<u64, Vec<Line>> = BTreeMap::new();

    println!("Parsing...");

    let filenames = glob("./logs/*.asm").unwrap();
    for filename in filenames.flatten() {
        let cap = filename_regex.captures(filename.to_str().unwrap()).unwrap();

        let bank_group = (
            u8::from_str_radix(&cap[1], 16).unwrap(),
            if let Some(c) = cap.get(3) {
                if c.as_str().trim().is_empty() {
                    u8::from_str_radix(&cap[1], 16).unwrap()
                } else {
                    u8::from_str_radix(c.as_str(), 16).unwrap()
                }
            } else {
                u8::from_str_radix(&cap[1], 16).unwrap()
            },
        );

        bank_groups.push(bank_group);

        let file = File::open(filename).unwrap();
        let reader = BufReader::new(file);
        let mut file_state = FileParsingState::new((u64::from(bank_group.0) << 16) + 0x8000);

        /* Parse the full file into data */
        for line in reader.lines() {
            let line = line.unwrap(); // TODO: Error handling
            let (parsed_addr, parsed_line) = Line::parse(&line, &config, &mut file_state);
            if let Some(addr) = parsed_addr {
                file_state.cur_addr = addr;
            }

            if let Line::Code(Code {
                instruction_prototype: Some(instr_proto),
                ..
            }) = &parsed_line
            {
                match global_state.labels.0.entry(file_state.cur_addr) {
                    Entry::Vacant(e) => {
                        e.insert(Label::new(
                            file_state.cur_addr,
                            LabelName::AutoGenerated(format!("SUB_{:06X}", file_state.cur_addr)),
                            LabelType::Instruction(instr_proto.clone()),
                            0,
                        ));
                    }
                    Entry::Occupied(e) => {
                        eprintln!("Duplicate prototype for label {}", e.get().name());
                    }
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

    println!("Indexing...");

    /* copy enemy-banks to respective new bank */
    let enemy_banks = [
        0xA2u8, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xB2, 0xB3,
    ];
    let mut new_lines = Vec::new();
    for (&addr, addr_lines) in lines.range(0xA0_8000..=0xA0_8686) {
        for bank in enemy_banks {
            let mut new_addr_lines = Vec::new();
            let new_addr = addr_with_bank(bank, addr);

            for line in addr_lines {
                let new_line = match line {
                    Line::Code(c) => {
                        if let Some(instr_proto) = &c.instruction_prototype {
                            match global_state.labels.0.entry(new_addr) {
                                Entry::Vacant(e) => {
                                    e.insert(Label::new(
                                        new_addr,
                                        LabelName::AutoGenerated(format!("SUB_{new_addr:06X}")),
                                        LabelType::Instruction(instr_proto.clone()),
                                        0,
                                    ));
                                }
                                Entry::Occupied(e) => {
                                    eprintln!("Duplicate prototype for label {}", e.get().name());
                                }
                            }
                        }

                        // Auto-add overrides to fix labels that referenced bank $A0
                        if let StaticAddress::DataBank(_low_addr) = c.get_operand() {
                            if let Some(0xA0) =
                                c.get_operand_label_address(None).map(|a| split_addr16(a).0)
                            {
                                config.add_overrides(Some(Override {
                                    db: Some(0xA0),
                                    ..Override::new(OverrideAddr::Address(new_addr))
                                }));
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

    /* Autogenerate labels */
    label::load_labels(&config, &mut global_state.labels);
    structs::generate_overrides(&mut config, &global_state.labels);
    label::generate_labels(&lines, &config, &mut global_state.labels);

    println!("Generating...");

    let mut output_file = File::create("./asm/main.asm").unwrap();
    let _ = writeln!(output_file, "lorom");
    let _ = writeln!(output_file, "incsrc labels.asm");
    for group in 0x80..0xE0 {
        let _ = writeln!(output_file, "incsrc bank_{group:02x}.asm");
    }

    let mut cur_bank = 0;
    for (addr, line) in &lines {
        let bank = (addr >> 16) as u8;

        if bank != cur_bank {
            let Some(first_entry) = lines.iter().find(|(k, v)| {
                **k >= ((u64::from(bank) << 16) | 0x8000)
                    && v.iter().any(|l| matches!(l, Line::Code(_) | Line::Data(_)))
            }) else {
                continue;
            };
            let first_address = if (first_entry.0 >> 16) == u64::from(bank) {
                first_entry.0
            } else {
                addr
            };

            cur_bank = bank;
            output_file = File::create(format!("./asm/bank_{cur_bank:02x}.asm")).unwrap();
            let _ = writeln!(output_file, "org ${first_address:06X}");
        }

        if let Some(label) = global_state.labels.0.get_mut(addr) {
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

        for addr_line in line {
            let _ = writeln!(
                output_file,
                "{}",
                addr_line.to_string(&config, &mut global_state.labels)
            );
        }
    }

    output_file = File::create("./asm/labels.asm").unwrap();
    for (a, l) in global_state
        .labels
        .iter()
        .filter(|(_, l)| !l.assigned.get() && !l.is_blocked())
    {
        let _ = writeln!(output_file, "{} = ${:06X}", l.name(), a);
    }

    output_file = File::create("./asm/all_labels.asm").unwrap();
    for (a, l) in global_state
        .labels
        .iter()
        .filter(|(_, l)| l.assigned.get() && !l.is_blocked())
    {
        let _ = writeln!(output_file, "{} = ${:06X}", l.name(), a);
    }

    output_file = File::create("./asm/externals.asm").unwrap();
    let _ = writeln!(output_file, "; Generated by sm_banklog_parser");
    for (a, l) in global_state
        .labels
        .iter()
        .filter(|(_, l)| l.assigned.get() && l.is_external() && !l.is_blocked())
    {
        let _ = writeln!(output_file, "{} = ${:06X}", l.name(), a);
    }
}
