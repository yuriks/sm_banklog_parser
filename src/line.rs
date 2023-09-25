use byteorder::{ByteOrder, LittleEndian};
use winnow::Parser;

use crate::code::Code;
use crate::config::Config;
use crate::data::{Data, DataVal};
use crate::directives::{parse_instruction_prototype, InstructionPrototype};
use crate::label::LabelMap;
use crate::opcode::OPCODES;
use crate::parse::{parse_sub_comment, ParsedCodeLine, ParsedDataLine, ParsedFillToLine};
use crate::{parse, Addr, Bank, FileParsingState, SpecialParsingType};

#[derive(Debug, Clone)]
pub enum Line {
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
            file_state
                .get_modifiers_mut()
                .set_data_type(SpecialParsingType::Spritemap)
                .unwrap();
        }
        "spritemap_raw" => {
            file_state
                .get_modifiers_mut()
                .set_data_type(SpecialParsingType::SpritemapRaw)
                .unwrap();
        }
        "spritemap_extended" => {
            file_state
                .get_modifiers_mut()
                .set_data_type(SpecialParsingType::SpritemapExtended)
                .unwrap();
        }
        "instruction_list" => {
            file_state
                .get_modifiers_mut()
                .set_data_type(SpecialParsingType::InstructionList)
                .unwrap();
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
    pub fn parse(
        line: &str,
        _config: &Config,
        file_state: &mut FileParsingState,
    ) -> (Option<Addr>, Line) {
        let special_type = file_state.get_modifiers().data_type;

        if let Ok(parsed) = parse::parse_comment_line.parse(line) {
            let mut addr = None;

            if let Some(rest) = parsed.strip_prefix(";@!") {
                if let Err(s) = process_directive(rest, file_state) {
                    eprintln!("{s}");
                }
            } else if let Ok(parsed) = parse_sub_comment.parse(parsed) {
                let (low_addr, _description) = parsed;
                addr = Some(file_state.addr_in_current_bank(low_addr));
            }
            (addr, Line::Comment(parsed.to_owned()))
        } else if let Ok((bracket, _)) = parse::parse_bracket_line.parse(line) {
            match bracket {
                '{' => file_state.push_context(),
                '}' => file_state.pop_context().unwrap(), // TODO: Error handling
                _ => unreachable!(),
            }
            (None, Line::Comment(line.to_owned()))
        } else if let Ok(parsed) = parse::parse_fillto_line.parse(line) {
            let ParsedFillToLine {
                line_addr,
                target,
                fill_byte,
                // TODO: Comment is being ignored
                comment: _,
            } = parsed;

            let line = Line::Comment(format!("padbyte ${fill_byte:02X} : pad ${target:06X}"));
            (Some(line_addr), line)
        } else if let Ok(parsed) = parse::parse_code_line.parse(line) {
            let ParsedCodeLine {
                line_addr,
                instruction_bytes,
                mnemonic: _,
                logged_address,
                comment,
            } = parsed;

            let logged_address = match logged_address {
                Some((Some(db), addr)) => Some((db, addr)),
                _ => None,
            };

            process_code_line(
                file_state,
                line_addr,
                &instruction_bytes,
                logged_address,
                comment,
                special_type,
            )
        } else if let Ok(parsed) = parse::parse_data_line.parse(line) {
            let ParsedDataLine {
                line_addr: address,
                data_type,
                data_values: data,
                comment,
            } = parsed;
            let comment = match comment {
                Some(c) if c.len() > 1 => Some(c.trim().to_owned()),
                _ => None,
            };

            file_state.last_data_cmd = data_type.to_string();

            let addr_offset: u64 = data.iter().map(|d| d.length()).sum();

            let mut lpc = address + addr_offset;
            if (lpc & 0xFFFF) < 0x8000 {
                lpc |= 0x8000;
            }
            file_state.last_pc = lpc;

            let line = Line::Data(Data {
                address,
                data,
                comment,
                special_type,
            });
            (Some(address), line)
        } else if let Ok(parsed) = parse::parse_data_line_continuation.parse(line) {
            let ParsedDataLine {
                line_addr: _,
                data_type: _,
                data_values: data,
                comment,
            } = parsed;
            let comment = match comment {
                Some(c) if c.len() > 1 => Some(c.trim().to_owned()),
                _ => None,
            };

            let address = file_state.last_pc;

            let addr_offset: u64 = data.iter().map(|d| d.length()).sum();

            let mut lpc = address + addr_offset;
            if (lpc & 0xFFFF) < 0x8000 {
                lpc |= 0x8000;
            }
            file_state.last_pc = lpc;

            let line = Line::Data(Data {
                address,
                data,
                comment,
                special_type,
            });
            (Some(address), line)
        } else {
            (None, Line::Comment(line.trim().to_string()))
        }
    }
}

fn process_code_line(
    file_state: &mut FileParsingState,
    address: Addr,
    opcodes: &[u8],
    logged_addr: Option<(Bank, u16)>,
    comment: Option<&str>,
    special_type: Option<SpecialParsingType>,
) -> (Option<Addr>, Line) {
    let opcode = &OPCODES[&opcodes[0]];
    let operand_size = (opcodes.len() - 1) as u8;
    let raw_operand: u32 = match operand_size {
        0 => 0,
        1 => u32::from(opcodes[1]),
        2 => u32::from(LittleEndian::read_u16(&opcodes[1..=2])),
        3 => LittleEndian::read_u24(&opcodes[1..=3]),
        _ => panic!("Invalid operand size"),
    };

    // The bank logs have various incorrect logged runtime addresses where 7E is reported as
    // the bank for IO port operations. Discard these to fall back to guessing and prevent
    // matching to incorrect labels.
    let logged_bank = logged_addr
        .filter(|(bank, addr)| !matches!((bank, addr), (0x7E, 0x2000..=0x5FFF)))
        .map(|(bank, _)| bank);

    let comment = comment.map(|c| c.to_owned());

    if opcode.name == "BRK" && operand_size == 0 {
        let data = Data {
            address,
            data: vec![DataVal::DB(0)],
            comment,
            special_type,
        };
        (Some(address), Line::Data(data))
    } else {
        let code = Code {
            address,
            opcode,
            raw_operand,
            operand_size,
            comment,
            logged_bank,
            instruction_prototype: file_state.prefixed_instruction_directive.take(),
        };
        (Some(address), Line::Code(code))
    }
}
