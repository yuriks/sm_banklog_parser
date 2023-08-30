use std::{collections::BTreeMap, fs::File, io::{BufRead, BufReader, Write}};

use glob::glob;
use regex::Regex;

use code::{ArgType, Code};
use data::Data;
use line::Line;

use crate::label::{LabelMap, LabelType};

mod code;
mod opcode;
mod data;
mod label;
mod line;
mod config;

fn is_bulk_data(addr: u32) -> bool {
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
    SpritemapExtended,
}

#[derive(Clone)]
struct ParsingModifiers {
    data_type: Option<SpecialParsingType>,
}

impl ParsingModifiers {
    fn set_data_type(&mut self, new_type: SpecialParsingType) -> Result<(), String> {
        match &self.data_type {
            None => {
                self.data_type = Some(new_type);
                Ok(())
            },
            Some(current_type) => Err(format!("Special data type already set: {:?}", current_type)),
        }
    }
}

impl Default for ParsingModifiers {
    fn default() -> Self {
        ParsingModifiers {
            data_type: None,
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

    last_data_cmd: String,
    last_pc: u64,
}

impl FileParsingState {
    fn new() -> Self {
        FileParsingState {
            modifier_stack: vec![ParsingModifiers::default()],
            last_data_cmd: String::new(),
            last_pc: 0,
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
}

fn main() -> Result<(), Box<dyn std::error::Error>> {

    let filename_regex = Regex::new(r"Bank \$([0-9A-F]{2})(\.\.\$([0-9A-F]{2})|)").unwrap();
    let mut bank_groups: Vec<(u8, u8)> = Vec::new();
    let config = config::Config::load("./config/");

    let mut global_state = GlobalParsingState::new();
    let mut lines: BTreeMap<u64, Vec<Line>> = BTreeMap::new();

    println!("Parsing...");

    let filenames = glob("./logs/*.asm").unwrap();
    for filename in filenames.flatten() {
        let cap = filename_regex.captures(filename.to_str().unwrap()).unwrap();
        
        let bank_group = 
            (u8::from_str_radix(&cap[1], 16).unwrap(), 
                if let Some(c) = cap.get(3) {
                    if !c.as_str().trim().is_empty() {
                        u8::from_str_radix(c.as_str(), 16).unwrap() 
                    } else {
                        u8::from_str_radix(&cap[1], 16).unwrap()                             
                    }
                } else { 
                    u8::from_str_radix(&cap[1], 16).unwrap() 
                }
            );

        bank_groups.push(bank_group);

        let file = File::open(filename).unwrap();
        let reader = BufReader::new(file);
        let mut cur_addr = 0x008000 | ((bank_group.0 as u64) << 16);
        let mut file_state = FileParsingState::new();
        
        /* Parse the full file into data */
        for (addr, line) in reader.lines().flatten().map(|l| Line::parse(&l, &config, &mut file_state)) {
            cur_addr = addr.unwrap_or(cur_addr);
            if !is_bulk_data(cur_addr as u32) {
                lines.entry(cur_addr).or_insert_with(Vec::new).push(line);
            }
        }
    }

    println!("Indexing...");
    
    /* copy enemy-banks to respective new bank */
    let enemy_banks = vec![0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xB2, 0xB3];
    let enemy_lines: BTreeMap<u64, Vec<Line>> = lines.iter().filter(|(k, _)| **k >= 0xA08000 && **k <= 0xA08686).map(|(k,v)| (*k, v.clone())).collect();
    for addr_line in &enemy_lines {
        for bank in &enemy_banks {
            let mut new_lines = Vec::new();
            let new_addr = bank << 16 | *addr_line.0 & (0xFFFF_u64);
            for line in addr_line.1 {
                let new_line = match line {
                    Line::Code(c) => {
                        let new_arg = match c.arg {
                            ArgType::Address(a) => {
                                if c.length == 3 && (new_addr & 0xFFFF) > 0x804D {
                                    ArgType::Address(bank << 16 | a & (0xFFFF_u64))
                                } else { ArgType::Address(a) }
                            },
                            ArgType::None => ArgType::None,
                            ArgType::BlockMove(a, b) => ArgType::BlockMove(a, b)
                        };

                        Line::Code(Code {
                            address: new_addr,
                            comment: c.comment.clone(),
                            length: c.length,
                            opcode: c.opcode,
                            db: *bank as u8,
                            arg: new_arg
                        })
                    },
                    Line::Data(d) => {
                        let new_data_addr = bank << 16 | d.address & (0xFFFF_u64);
                        Line::Data(Data {
                            address: new_data_addr,
                            comment: d.comment.clone(),
                            data: d.data.clone(),
                            special_type: d.special_type,
                        })
                    },
                    Line::Comment(c) => Line::Comment(c.to_string())
                };

                new_lines.push(new_line);
            }

            lines.insert(new_addr, new_lines);
        }
    }

    /* Autogenerate labels */
    label::generate_labels(&lines, &config, &mut global_state.labels);

    println!("Generating...");

    let mut output_file = File::create("./asm/main.asm").unwrap();
    let _ = writeln !(output_file, "lorom");
    let _ = writeln!(output_file, "incsrc labels.asm");
    for group in 0x80..0xE0 {
        let _ = writeln!(output_file, "incsrc bank_{:02x}.asm", group);
    }

    let mut cur_bank = 0;
    for (addr, line) in &lines {
        let bank = (addr >> 16) as u8;

        if bank != cur_bank {
            let first_entry = if let Some(e) = lines.iter().find(|(k, v)| **k >= (((bank as u64) << 16) | 0x8000) && v.iter().any(|l| matches!(l, Line::Code(_) | Line::Data(_)))) {
                e
            } else {
                continue;
            };
            let first_address = if (first_entry.0 >> 16) == bank as u64 { first_entry.0 } else { addr };

            cur_bank = bank;
            output_file = File::create(format!("./asm/bank_{:02x}.asm", cur_bank)).unwrap();
            let _ = writeln!(output_file, "org ${:06X}", first_address);
        }
        
        if let Some(label) = global_state.labels.get_mut(addr) {
            writeln!(output_file, "{}{}", label.name, if label.name.starts_with(".") { "" } else { ":" }).unwrap();
            label.assigned = true;
        }

        for addr_line in line {
            let _ = writeln!(output_file, "{}", addr_line.to_string(&config, &mut global_state.labels));
        }

    }

    output_file = File::create("./asm/labels.asm").unwrap();
    for (a, l) in global_state.labels.iter().filter(|(_,l)| !l.assigned && l.label_type != LabelType::Blocked) {
        let _ = writeln!(output_file, "{} = ${:06X}", l.name, a);
    }

    output_file = File::create("./asm/all_labels.asm").unwrap();
    for (a, l) in global_state.labels.iter().filter(|(_,l)| l.assigned && l.label_type != LabelType::Blocked) {
        let _ = writeln!(output_file, "{} = ${:06X}", l.name, a);
    }

    output_file = File::create("./asm/externals.asm").unwrap();
    let _ = writeln!(output_file, "; Generated by sm_banklog_parser");
    for (a, l) in global_state.labels.iter().filter(|(_,l)| l.assigned && l.external && l.label_type != LabelType::Blocked) {
        let _ = writeln!(output_file, "{} = ${:06X}", l.name, a);
    }

    Ok(())
}
