use byteorder::{ByteOrder, LittleEndian};
use lazy_static::lazy_static;
use regex::Regex;

use crate::{Addr, FileParsingState, SpecialParsingType};
use crate::code::{ArgType, Code};
use crate::config::Config;
use crate::data::{Data, DataVal};
use crate::directives::{InstructionPrototype, parse_instruction_prototype};
use crate::label::LabelMap;
use crate::opcode::OPCODES;

/* Compile these into static variables once at runtime for performance reasons */
lazy_static! {
    static ref CODE_REGEX: Regex = Regex::new(r"^\$([0-9A-F]{2}:[0-9A-F]{4})\s*(([0-9A-F]{2} ?)+)\s*([A-Z]{3})\s*(([#\$A-F0-9sxy,()\[\]])*?)\s*((\[\$([0-9A-F]{4}|[0-9A-F]{2}:[0-9A-F]{4})\])*)\s*(;.*)*$").unwrap();
    static ref BLOCKMOVE_REGEX: Regex = Regex::new(r"^\$([0-9A-F]{2}:[0-9A-F]{4})\s*(([0-9A-F]{2} ?)+)\s*(MVN|MVP) [0-9A-F]{2} [0-9A-F]{2}\s*((\[\$([0-9A-F]{4}|[0-9A-F]{2}:[0-9A-F]{4})\])*)\s*(;.*)*$").unwrap();
    static ref COMMENT_REGEX: Regex = Regex::new(r"^\s*(;.*)$").unwrap();
    static ref DATA_START_REGEX: Regex = Regex::new(r"^\$([0-9A-F]{2}:[0-9A-F]{4})(/\$[0-9A-F]{4}|)\s*(|db|dw|dl|dx|dW)\s*((([A-F0-9]*),\s*)*([A-F0-9]*))(\s*$|\s*;)(.*)$").unwrap();
    static ref DATA_CONT_REGEX: Regex = Regex::new(r"^\s+((([A-F0-9]*),\s*)*([A-F0-9]*))(\s*$|\s*;)(.*)$").unwrap();
    static ref SUB_REGEX: Regex = Regex::new(r"^;;; \$(?P<addr>[[:xdigit:]]+):\s*(?P<desc>.*?)\s*(?:;;;)?\s*$").unwrap();
    static ref FILL_REGEX: Regex = Regex::new(r"^(.*?)fillto \$([A-F0-9]*)\s*,\s*\$([A-F0-9]*)\s*.*$").unwrap();
    static ref BRACKETS_REGEX: Regex = Regex::new(r"^\s*([{}])\s*(;.*)?$").unwrap();
}

#[derive(Debug, Clone)]
pub enum Line
{
    // Also used for raw text in other unhandled lines
    Comment(String),
    Data(Data),
    Code(Code),
}

impl Line {
    pub fn to_string(&self, config: &Config, labels: &mut LabelMap) -> String {
        match self {
            Line::Comment(s) => s.to_string(),
            Line::Data(d) => d.to_string(config, labels),
            Line::Code(c) => c.to_string(config, labels),
        }
    }
}

fn process_directive(line: &str, file_state: &mut FileParsingState) -> Result<(), String> {
    let (cmd, args) = match line.split_once(' ') {
        Some((cmd, args)) => (cmd, Some(args)),
        None => (line, None),
    };

    match cmd {
        "spritemap" => {
            file_state.get_modifiers_mut().set_data_type(SpecialParsingType::Spritemap).unwrap();
        }
        "spritemap_raw" => {
            file_state.get_modifiers_mut().set_data_type(SpecialParsingType::SpritemapRaw).unwrap();
        }
        "spritemap_extended" => {
            file_state.get_modifiers_mut().set_data_type(SpecialParsingType::SpritemapExtended).unwrap();
        }
        "instruction_list" => {
            file_state.get_modifiers_mut().set_data_type(SpecialParsingType::InstructionList).unwrap();
        }
        "instruction" => {
            if file_state.prefixed_instruction_directive.is_some() {
                return Err("Duplicated instruction directive.".into());
            }
            let args = args.ok_or("Missing instruction parameter list")?;
            let params = parse_instruction_prototype(args)?;

            file_state.prefixed_instruction_directive = Some(InstructionPrototype { params });
        }
        _ => return Err(format!("Unknown parsing directive: {}", line)),
    }

    Ok(())
}

impl Line {
    pub fn parse(line: &str, _config: &Config, file_state: &mut FileParsingState) -> (Option<Addr>, Line) {
        let special_type = file_state.get_modifiers().data_type;

        if let Some(cap) = SUB_REGEX.captures(line) {
            let addr = u16::from_str_radix(&cap["addr"], 16).unwrap(); // TODO: Error handling
            let _desc = &cap["desc"];
            let full_addr = file_state.addr_in_current_bank(addr);
            (Some(full_addr), Line::Comment(line.into()))
        } else if let Some(cap) = COMMENT_REGEX.captures(line) {
            if let Some(rest) = line.strip_prefix(";@!") {
                if let Err(s) = process_directive(rest, file_state) {
                    eprintln!("{}", s);
                }
            }
            (None, Line::Comment(cap[1].into()))
        } else if let Some(cap) = BRACKETS_REGEX.captures(line) {
            match &cap[1] {
                "{" => file_state.push_context(),
                "}" => file_state.pop_context().unwrap(), // TODO: Error handling
                _ => unreachable!(),
            }
            (None, Line::Comment(line.into()))
        } else if let Some(cap) = FILL_REGEX.captures(line) {
            let (raw_target, raw_pad_byte) = (&cap[2], &cap[3]);
            let target = u64::from_str_radix(raw_target, 16).unwrap();
            let pad_byte = u8::from_str_radix(raw_pad_byte, 16).unwrap();
            (None, Line::Comment(format!("padbyte ${:02X} : pad ${:06X}", pad_byte, target)))
        } else if let Some(cap) = BLOCKMOVE_REGEX.captures(line) {
            let (raw_addr, raw_opcode, comment) = (&cap[1], &cap[2], cap.get(9));
            let address: u64 = u64::from_str_radix(&raw_addr.replace(':', ""), 16).unwrap();
            let opcodes: Vec<u8> = raw_opcode.trim().split(' ').map(|o| u8::from_str_radix(o, 16).unwrap()).collect();
            let opcode = &OPCODES[&opcodes[0]];
            let arg = ArgType::BlockMove(opcodes[1], opcodes[2]);

            let code = Code {
                address,
                opcode,
                arg,
                length: 3,
                db: (address >> 16) as u8,
                comment: comment.map(|c| c.as_str()[1..].to_owned()),
                instruction_prototype: file_state.prefixed_instruction_directive.take(),
            };

            (Some(address), Line::Code(code))
        } else if let Some(cap) = CODE_REGEX.captures(line) {
            let (raw_addr, raw_opcode, _op_name, _op_arg, op_db, comment) = (&cap[1], &cap[2], &cap[4], &cap[5], cap.get(8), cap.get(10));
            let address: u64 = u64::from_str_radix(&raw_addr.replace(':', ""), 16).unwrap();
            let opcodes: Vec<u8> = raw_opcode.trim().split(' ').map(|o| u8::from_str_radix(o, 16).unwrap()).collect();
            let mut arg_addr: u64 = 0;
            let opcode = &OPCODES[&opcodes[0]];
            let length = (opcodes.len() - 1) as u8;

            let arg = {
                if opcodes.len() == 1 {
                    ArgType::None
                } else {
                    arg_addr = match length {
                        1 => opcodes[1] as u64,
                        2 => LittleEndian::read_u16(&opcodes[1..3]) as u64,
                        3 => LittleEndian::read_u24(&opcodes[1..4]) as u64,
                        _ => panic!("Invalid opcode length")
                    };

                    ArgType::Address(arg_addr)
                }
            };

            let db = {
                if let Some(db) = op_db {
                    if db.as_str().contains(':') && (arg_addr & 0xFFFF) > 0x8000 {
                        u8::from_str_radix(&db.as_str()[2..4], 16).unwrap()
                    } else {
                        ((address >> 16) & 0xFF) as u8
                    }
                } else {
                    ((address >> 16) & 0xFF) as u8
                }
            };

            let comment = comment.map(|c| c.as_str()[1..].to_owned());

            let code = Code {
                address,
                opcode,
                arg,
                length,
                db,
                comment: comment.clone(),
                instruction_prototype: file_state.prefixed_instruction_directive.take(),
            };

            if code.opcode.name == "BRK" && code.length == 0 {
                (Some(address), Line::Data(Data { address, data: vec![DataVal::DB(0)], comment, special_type }))
            } else {
                (Some(address), Line::Code(code))
            }
        } else if let Some(cap) = DATA_START_REGEX.captures(line) {
            let (raw_addr, data_type, raw_data, raw_comment) = (&cap[1], &cap[3], &cap[4], cap.get(9));
            let address: u64 = u64::from_str_radix(&raw_addr.replace(':', ""), 16).unwrap();
            let data: Vec<u64> = raw_data.split(',').map(|d| d.trim()).filter(|d| !d.is_empty()).map(|d| u64::from_str_radix(d, 16).unwrap()).collect();

            let comment = match raw_comment {
                Some(c) if c.as_str().len() > 1 => Some(c.as_str().trim().to_owned()),
                _ => None
            };

            {
                file_state.last_data_cmd = data_type.to_string();
            }

            let data_type = "dx";

            let data = match data_type.to_lowercase().as_str() {
                "db" => data.iter().map(|d| DataVal::DB(*d as u8)).collect(),
                "dw" => data.iter().map(|d| DataVal::DW(*d as u16)).collect(),
                "dl" => data.iter().map(|d| DataVal::DL(*d as u32)).collect(),
                "dx" => {
                    let mut dx_data: Vec<DataVal> = Vec::new();
                    for d in raw_data.split(',').map(|d| d.trim()).filter(|d| !d.is_empty()) {
                        match d.len() {
                            2 => dx_data.push(DataVal::DB(u8::from_str_radix(d, 16).unwrap())),
                            4 => dx_data.push(DataVal::DW(u16::from_str_radix(d, 16).unwrap())),
                            6 => dx_data.push(DataVal::DL(u32::from_str_radix(d, 16).unwrap())),
                            8 => {
                                dx_data.push(DataVal::DW(u16::from_str_radix(&d[0..2], 16).unwrap()));
                                dx_data.push(DataVal::DW(u16::from_str_radix(&d[2..4], 16).unwrap()))
                            },
                            _ => panic!("Invalid dx value length: {:?}", line)
                        }
                    }
                    dx_data
                },
                _ => panic!("Unknown data type")
            };

            let addr_offset: u64 = data.iter().map(|d| d.length()).sum();

            let mut lpc = address + addr_offset;
            if (lpc & 0xFFFF) < 0x8000 {
                lpc |= 0x8000;
            }
            file_state.last_pc = lpc;

            (Some(address), Line::Data(Data { address, data, comment, special_type }))
        } else if let Some(cap) = DATA_CONT_REGEX.captures(line) {
            let (raw_data, raw_comment) = (&cap[1], cap.get(6));

            let comment = match raw_comment {
                Some(c) if c.as_str().len() > 1 => Some(c.as_str().trim().to_owned()),
                _ => None
            };

            if raw_data.trim().len() > 1 {
                let data: Vec<u64> = raw_data.split(',').map(|d| d.trim()).filter(|d| !d.is_empty()).map(|d| u64::from_str_radix(d, 16).unwrap()).collect();
                let (_data_type, address) = {
                    (file_state.last_data_cmd.clone(), file_state.last_pc)
                };

                let data_type = "dx";

                let data = match data_type.to_lowercase().as_str() {
                    "db" => data.iter().map(|d| DataVal::DB(*d as u8)).collect(),
                    "dw" => data.iter().map(|d| DataVal::DW(*d as u16)).collect(),
                    "dl" => data.iter().map(|d| DataVal::DL(*d as u32)).collect(),
                    "dx" => {
                        let mut dx_data: Vec<DataVal> = Vec::new();
                        for d in raw_data.split(',').map(|d| d.trim()).filter(|d| !d.is_empty()) {
                            match d.len() {
                                2 => dx_data.push(DataVal::DB(u8::from_str_radix(d, 16).unwrap())),
                                4 => dx_data.push(DataVal::DW(u16::from_str_radix(d, 16).unwrap())),
                                6 => dx_data.push(DataVal::DL(u32::from_str_radix(d, 16).unwrap())),
                                8 => {
                                    dx_data.push(DataVal::DW(u16::from_str_radix(&d[0..2], 16).unwrap()));
                                    dx_data.push(DataVal::DW(u16::from_str_radix(&d[2..4], 16).unwrap()))
                                },
                                _ => panic!("Invalid dx value length")
                            }
                        }
                        dx_data
                    },
                    _ => panic!("Unknown data type")
                };

                let addr_offset: u64 = data.iter().map(|d| d.length()).sum();

                let mut lpc = address + addr_offset;
                if (lpc & 0xFFFF) < 0x8000 {
                    lpc |= 0x8000;
                }
                file_state.last_pc = lpc;

                (Some(address), Line::Data(Data { address, data, comment, special_type }))
            } else {
                (None, Line::Comment(raw_data.trim().to_string()))
            }
        } else {
            (None, Line::Comment(line.to_string()))
        }
    }
}

