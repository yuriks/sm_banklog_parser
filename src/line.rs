use byteorder::{ByteOrder, LittleEndian};
use std::fmt::Write;
use std::iter;
use winnow::Parser;

use crate::code::Code;
use crate::config::Config;
use crate::data::{Data, DataVal};
use crate::directives::{parse_instruction_prototype, InstructionPrototype};
use crate::label::LabelMap;
use crate::opcode::OPCODES;
use crate::parse::{parse_sub_comment, ParsedCodeLine, ParsedDataLine, ParsedFillToLine};
use crate::{parse, split_addr16, Addr, Bank, FileParsingState, SpecialParsingType};

#[derive(Debug, Clone)]
pub enum LineContent {
    Raw(String),
    Data(Data),
    Code(Code),
}

#[derive(Debug, Clone)]
pub struct Line {
    pub address: Option<Addr>,
    pub contents: LineContent,
    pub comment: Option<String>,
}

impl LineContent {
    pub fn pc_advance(&self) -> u64 {
        match self {
            LineContent::Raw(_) => 0,
            LineContent::Data(d) => d.pc_advance(),
            LineContent::Code(c) => c.pc_advance(),
        }
    }

    /// True if the line emit output bytes. Equivalent to `self.pc_advance() != 0`.
    pub fn produces_output(&self) -> bool {
        match self {
            LineContent::Data(_) | LineContent::Code(_) => true,
            LineContent::Raw(_) => false,
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
    pub fn new_comment(s: impl Into<String>) -> Line {
        Line {
            address: None,
            contents: LineContent::Raw(String::new()),
            comment: Some(s.into()),
        }
    }

    pub fn with_address(mut self, address: Addr) -> Line {
        self.address = Some(address);
        match &mut self.contents {
            LineContent::Raw(_) => {}
            LineContent::Data(data) => data.address = address,
            LineContent::Code(code) => code.address = address,
        };
        self
    }

    pub fn to_string(&self, config: &Config, labels: &LabelMap) -> String {
        let add_address_to_comment =
            matches!(self.contents, LineContent::Data(_) | LineContent::Code(_));
        let (mut output, extra_lines) = match &self.contents {
            LineContent::Raw(s) => (s.trim_end().to_owned(), Vec::new()),
            LineContent::Data(d) => d.to_string(config, labels),
            LineContent::Code(c) => (c.to_string(config, labels), Vec::new()),
        };

        let mut it = extra_lines.into_iter();
        if output.is_empty() {
            output = it.next().unwrap_or_default();
        }

        fn pad_to_width(width: usize, s: &mut String) {
            let padding_needed = width.saturating_sub(s.chars().count());
            s.extend(iter::repeat(' ').take(padding_needed));
        }

        if add_address_to_comment || self.comment.is_some() {
            if !output.is_empty() {
                pad_to_width(4 + 40 - 1, &mut output);
                output.push(' ');
            }
            output.push(';');

            if add_address_to_comment {
                write!(output, " ${:06X} |", self.address.unwrap()).unwrap();
            }
            if let Some(comment) = self.comment.as_ref().filter(|s| !s.is_empty()) {
                if add_address_to_comment {
                    output.push(' ');
                }
                output.push_str(comment);
            }
        }

        for line in it {
            output.push('\n');
            output.push_str(&line);
        }

        output
    }

    pub fn parse(line: &str, file_state: &mut FileParsingState) -> Line {
        let special_type = file_state.get_modifiers().data_type;

        let result = if let Ok(parsed) = parse::parse_comment_line.parse(line) {
            let mut address = None;

            if let Some(rest) = parsed.strip_prefix("@!") {
                if let Err(s) = process_directive(rest, file_state) {
                    eprintln!("{s}");
                }
            } else if let Ok(parsed) = parse_sub_comment.parse(parsed) {
                let (low_addr, _description) = parsed;
                let addr = file_state.addr_in_current_bank(low_addr);
                address = Some(addr);
                file_state.cur_addr = addr;
            }

            Line {
                address,
                contents: LineContent::Raw(String::new()),
                comment: Some(parsed.trim_end().to_owned()),
            }
        } else if let Ok((bracket, _)) = parse::parse_bracket_line.parse(line) {
            match bracket {
                '{' => file_state.push_context(),
                '}' => file_state.pop_context().unwrap(),
                _ => unreachable!(),
            }

            Line {
                address: None,
                contents: LineContent::Raw(line.to_owned()),
                comment: None,
            }
        } else if let Ok(parsed) = parse::parse_fillto_line.parse(line) {
            let ParsedFillToLine {
                line_addr,
                target,
                fill_byte,
                // TODO: Comment is being ignored
                comment: _,
            } = parsed;

            file_state.cur_addr = line_addr;

            Line {
                address: Some(line_addr),
                contents: LineContent::Raw(format!("padbyte ${fill_byte:02X} : pad ${target:06X}")),
                comment: None,
            }
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

            file_state.cur_addr = line_addr;
            file_state.cur_addr += instruction_bytes.len() as u64;

            Line {
                address: Some(line_addr),
                contents: process_code_line(
                    file_state,
                    line_addr,
                    &instruction_bytes,
                    logged_address,
                    special_type,
                ),
                comment: comment.map(|s| s.trim_end().to_owned()),
            }
        } else if let Ok(parsed) = parse::parse_data_line.parse(line) {
            let ParsedDataLine {
                line_addr,
                data_type,
                data_values: data,
                comment,
            } = parsed;

            file_state.last_data_cmd = data_type.to_string();
            let addr_offset: u64 = data.iter().map(|d| d.length()).sum();

            file_state.cur_addr = line_addr;
            file_state.cur_addr += addr_offset;

            Line {
                address: Some(line_addr),
                contents: LineContent::Data(Data {
                    address: line_addr,
                    data,
                    special_type,
                }),
                comment: comment.map(|s| s.trim().to_owned()),
            }
        } else if let Ok(parsed) = parse::parse_data_line_continuation.parse(line) {
            let ParsedDataLine {
                line_addr: _,
                data_type: _,
                data_values: data,
                comment,
            } = parsed;

            let line_addr = file_state.cur_addr;

            let addr_offset: u64 = data.iter().map(|d| d.length()).sum();
            file_state.cur_addr += addr_offset;

            Line {
                address: Some(line_addr),
                contents: LineContent::Data(Data {
                    address: line_addr,
                    data,
                    special_type,
                }),
                comment: comment.map(|s| s.trim().to_owned()),
            }
        } else {
            Line {
                address: None,
                contents: LineContent::Raw(line.to_owned()),
                comment: None,
            }
        };

        if let Some(address) = result.address {
            let a = address + result.contents.pc_advance();
            let b = file_state.cur_addr;
            assert_eq!(
                a, b,
                "Incorrect PC advance. Expected ${a:06X} but got ${b:06X}"
            );
        }

        // Handle bank-crossing wrap around
        if split_addr16(file_state.cur_addr).1 < 0x8000 {
            file_state.cur_addr |= 0x8000;
        }

        result
    }
}

fn process_code_line(
    file_state: &mut FileParsingState,
    address: Addr,
    opcodes: &[u8],
    logged_addr: Option<(Bank, u16)>,
    special_type: Option<SpecialParsingType>,
) -> LineContent {
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

    if opcode.name == "BRK" && operand_size == 0 {
        LineContent::Data(Data {
            address,
            data: vec![DataVal::DB(0)],
            special_type,
        })
    } else {
        LineContent::Code(Code {
            address,
            opcode,
            raw_operand,
            operand_size,
            logged_bank,
            instruction_prototype: file_state.prefixed_instruction_directive.take(),
        })
    }
}
