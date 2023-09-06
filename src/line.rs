use byteorder::{ByteOrder, LittleEndian};
use lazy_static::lazy_static;
use regex::Regex;

use crate::{Addr, Bank, FileParsingState, SpecialParsingType};
use crate::code::Code;
use crate::config::Config;
use crate::data::{Data, DataVal};
use crate::directives::{InstructionPrototype, parse_instruction_prototype};
use crate::label::{canonicalize_bank, LabelMap};
use crate::opcode::{AddressingBank, AddrMode, OPCODES};

/* Compile these into static variables once at runtime for performance reasons */
lazy_static! {
    static ref CODE_REGEX: Regex = Regex::new(r"^\$([0-9A-F]{2}:[0-9A-F]{4})\s*((?: ?[0-9A-F]{2})+)\s*([A-Z]{3})\s*([#\$A-F0-9sxy,()\[\]]*?)\s*(\[\$(?:([0-9A-F]{2}):)?([0-9A-F]{4})\])?\s*(;.*)*$").unwrap();
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
        _ => return Err(format!("Unknown parsing directive: {line}")),
    }

    Ok(())
}

impl Line {
    pub fn parse(line: &str, _config: &Config, file_state: &mut FileParsingState) -> (Option<Addr>, Line) {
        let special_type = file_state.get_modifiers().data_type;

        if let Some(cap) = SUB_REGEX.captures(line) {
            let addr = u16::from_str_radix(&cap["addr"], 16).unwrap(); // TODO: Error handling
            // let _desc = &cap["desc"];
            let full_addr = file_state.addr_in_current_bank(addr);
            (Some(full_addr), Line::Comment(line.into()))
        } else if let Some(cap) = COMMENT_REGEX.captures(line) {
            if let Some(rest) = line.strip_prefix(";@!") {
                if let Err(s) = process_directive(rest, file_state) {
                    eprintln!("{s}");
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
            let target = Addr::from_str_radix(raw_target, 16).unwrap();
            let pad_byte = u8::from_str_radix(raw_pad_byte, 16).unwrap();
            (None, Line::Comment(format!("padbyte ${pad_byte:02X} : pad ${target:06X}")))
        } else if let Some(cap) = BLOCKMOVE_REGEX.captures(line) {
            let (raw_addr, raw_opcode, comment) = (&cap[1], &cap[2], cap.get(9));
            let address: Addr = Addr::from_str_radix(&raw_addr.replace(':', ""), 16).unwrap();
            let opcodes: Vec<u8> = raw_opcode.trim().split(' ').map(|o| u8::from_str_radix(o, 16).unwrap()).collect();

            process_code_line(file_state, address, &opcodes, None, comment.map(|c| c.as_str()), None)
        } else if let Some(cap) = CODE_REGEX.captures(line) {
            let (raw_addr, raw_opcode, _op_name, _op_arg, op_logged_db, op_logged_addr, comment) = (&cap[1], &cap[2], &cap[3], &cap[4], cap.get(6), cap.get(7), cap.get(8));
            let address: Addr = Addr::from_str_radix(&raw_addr.replace(':', ""), 16).unwrap();
            let opcodes: Vec<u8> = raw_opcode.trim().split(' ').map(|o| u8::from_str_radix(o, 16).unwrap()).collect();

            let logged_addr = match (op_logged_db, op_logged_addr) {
                (Some(op_logged_db), Some(op_logged_addr)) => {
                    Some((
                        Bank::from_str_radix(op_logged_db.as_str(), 16).unwrap(),
                        Addr::from_str_radix(op_logged_addr.as_str(), 16).unwrap()))
                },
                _ => None,
            };

            process_code_line(file_state, address, &opcodes, logged_addr, comment.map(|c| c.as_str()), special_type)
        } else if let Some(cap) = DATA_START_REGEX.captures(line) {
            let (raw_addr, data_type, raw_data, raw_comment) = (&cap[1], &cap[3], &cap[4], cap.get(9));
            let address: Addr = Addr::from_str_radix(&raw_addr.replace(':', ""), 16).unwrap();
            let data: Vec<u64> = raw_data.split(',').map(str::trim).filter(|d| !d.is_empty()).map(|d| u64::from_str_radix(d, 16).unwrap()).collect();

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
                    for d in raw_data.split(',').map(str::trim).filter(|d| !d.is_empty()) {
                        match d.len() {
                            2 => dx_data.push(DataVal::DB(u8::from_str_radix(d, 16).unwrap())),
                            4 => dx_data.push(DataVal::DW(u16::from_str_radix(d, 16).unwrap())),
                            6 => dx_data.push(DataVal::DL(u32::from_str_radix(d, 16).unwrap())),
                            8 => {
                                dx_data.push(DataVal::DW(u16::from_str_radix(&d[0..2], 16).unwrap()));
                                dx_data.push(DataVal::DW(u16::from_str_radix(&d[2..4], 16).unwrap()));
                            },
                            _ => panic!("Invalid dx value length: {line:?}")
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
                let data: Vec<u64> = raw_data.split(',').map(str::trim).filter(|d| !d.is_empty()).map(|d| u64::from_str_radix(d, 16).unwrap()).collect();
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
                        for d in raw_data.split(',').map(str::trim).filter(|d| !d.is_empty()) {
                            match d.len() {
                                2 => dx_data.push(DataVal::DB(u8::from_str_radix(d, 16).unwrap())),
                                4 => dx_data.push(DataVal::DW(u16::from_str_radix(d, 16).unwrap())),
                                6 => dx_data.push(DataVal::DL(u32::from_str_radix(d, 16).unwrap())),
                                8 => {
                                    dx_data.push(DataVal::DW(u16::from_str_radix(&d[0..2], 16).unwrap()));
                                    dx_data.push(DataVal::DW(u16::from_str_radix(&d[2..4], 16).unwrap()));
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

fn process_code_line(file_state: &mut FileParsingState, address: Addr, opcodes: &[u8], logged_addr: Option<(Bank, Addr)>, comment: Option<&str>, special_type: Option<SpecialParsingType>) -> (Option<Addr>, Line) {
    let opcode = &OPCODES[&opcodes[0]];
    let operand_size = (opcodes.len() - 1) as u8;
    let raw_operand: u32 = match operand_size {
        0 => 0,
        1 => u32::from(opcodes[1]),
        2 => u32::from(LittleEndian::read_u16(&opcodes[1..=2])),
        3 => LittleEndian::read_u24(&opcodes[1..=3]),
        _ => panic!("Invalid operand size")
    };

    // The bank logs have various incorrect logged runtime addresses where 7E is reported as
    // the bank for IO port operations. Discard these to fall back to guessing and prevent
    // matching to incorrect labels.
    let logged_bank = logged_addr
        .filter(|(bank, addr)| !matches!((bank, addr), (0x7E, 0x2000..=0x5FFF)))
        .map(|(bank, _)| bank);

    let code_bank = (address >> 16) as Bank;
    let label_bank = match opcode.addr_mode.label_bank_source() {
        AddressingBank::None => None,
        // TODO: These need to be replaced by better instruction emulation to handle PC-relative, etc.
        AddressingBank::Data => Some((logged_bank.unwrap_or(code_bank), raw_operand & 0xFFFF)),
        AddressingBank::Program => Some((code_bank, raw_operand & 0xFFFF)),
        AddressingBank::Direct => Some((0, raw_operand & 0xFFFF)),
        AddressingBank::Long => Some(((raw_operand >> 16) as u8, raw_operand & 0xFFFF)),
        AddressingBank::IndirectLong => unreachable!("IndirectLong should never be a label bank source"),
    };

    let canonical_bank = if opcode.addr_mode == AddrMode::AbsoluteLongIndexed
        && (0x80..=0xCF).contains(&(raw_operand >> 16))
        && (raw_operand & 0xFFFF) < 0x40 {
        // This is sometimes used for accessing struct fields relative to a pointer to
        // another bank, which would be incorrectly canonicalized as a WRAM mirror.
        label_bank.map(|(bank, _addr)| bank)
    } else {
        label_bank.map(|(bank, addr)| canonicalize_bank((Addr::from(bank) << 16) + Addr::from(addr)))
    };

    let comment = comment.map(|c| c[1..].to_owned());

    if opcode.name == "BRK" && operand_size == 0 {
        (Some(address), Line::Data(Data { address, data: vec![DataVal::DB(0)], comment, special_type }))
    } else {
        (Some(address), Line::Code(Code {
            address,
            opcode,
            raw_operand,
            operand_size,
            comment,
            // TODO: Make this an Option?
            db: canonical_bank.unwrap_or(0xFF),
            instruction_prototype: file_state.prefixed_instruction_directive.take(),
        }))
    }
}
