use std::fmt::Write;
use std::iter;

use byteorder::{ByteOrder, LittleEndian};
use winnow::Parser;

use crate::code::Code;
use crate::data::{Data, DataVal};
use crate::directives::{parse_instruction_prototype, InstructionPrototype};
use crate::label::LabelMap;
use crate::opcode::Opcode;
use crate::operand::OverrideMap;
use crate::parse::ParsedCodeLine;
use crate::{parse, split_addr16, Addr, Bank, FileParsingState, SpecialParsingType};

#[derive(Debug, Clone)]
pub enum LineContent {
    Empty,
    /// May only be '{' or '}'.
    Bracket(char),
    SubMarker(Addr, String),
    Data(Data),
    Code(Code),
    FillTo(FillTo),
}

#[derive(Debug, Clone)]
pub struct Line {
    pub contents: LineContent,
    pub comment: Option<String>,
}

impl LineContent {
    pub fn pc_advance(&self) -> Addr {
        match self {
            LineContent::Empty | LineContent::Bracket(_) | LineContent::SubMarker(..) => 0,
            LineContent::Data(d) => d.pc_advance(),
            LineContent::Code(c) => c.pc_advance(),
            LineContent::FillTo(f) => f.pc_advance(),
        }
    }

    /// True if the line emits output bytes.
    pub fn produces_output(&self) -> bool {
        match self {
            LineContent::Data(_) | LineContent::Code(_) | LineContent::FillTo(..) => true,
            LineContent::Empty | LineContent::Bracket(_) | LineContent::SubMarker(..) => false,
        }
    }

    pub fn address(&self) -> Option<Addr> {
        match self {
            LineContent::Empty | LineContent::Bracket(_) => None,
            LineContent::SubMarker(a, _) => Some(*a),
            LineContent::Data(d) => Some(d.address),
            LineContent::Code(c) => Some(c.address),
            LineContent::FillTo(f) => Some(f.address),
        }
    }

    pub fn with_address(mut self, address: Addr) -> LineContent {
        match &mut self {
            LineContent::Empty | LineContent::Bracket(_) => {}
            LineContent::SubMarker(addr, _) => *addr = address,
            LineContent::Data(data) => data.address = address,
            LineContent::Code(code) => code.address = address,
            LineContent::FillTo(fillto) => fillto.address = address,
        };
        self
    }
}

#[derive(Debug, Clone)]
pub struct FillTo {
    pub address: Addr,
    pub target: Addr,
    pub fill_byte: u8,
}

impl FillTo {
    fn pc_advance(&self) -> Addr {
        self.target.checked_sub(self.address).unwrap()
    }

    #[allow(clippy::inherent_to_string)] // This should maybe have a better name
    fn to_string(&self) -> String {
        format!("padbyte ${:02X} : pad ${:06X}", self.fill_byte, self.target)
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
            contents: LineContent::Empty,
            comment: Some(s.into()),
        }
    }

    pub fn address(&self) -> Option<Addr> {
        self.contents.address()
    }

    pub fn with_address(mut self, address: Addr) -> Line {
        self.contents = self.contents.with_address(address);
        self
    }

    pub fn to_string(&self, overrides: &OverrideMap, labels: &LabelMap) -> String {
        let add_address_to_comment =
            matches!(self.contents, LineContent::Data(_) | LineContent::Code(_));
        let (mut output, extra_lines) = match &self.contents {
            LineContent::Empty | LineContent::SubMarker(..) => (String::new(), Vec::new()),
            LineContent::Bracket(c) => (c.to_string(), Vec::new()),
            LineContent::Data(d) => d.to_string(overrides, labels),
            LineContent::Code(c) => (c.to_string(overrides, labels), Vec::new()),
            LineContent::FillTo(f) => (f.to_string(), Vec::new()),
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
                let (bank, low_addr) = split_addr16(self.address().unwrap());
                write!(output, " ${bank:02X}:{low_addr:04X} ;").unwrap();
            }
            if let Some(comment) = self.comment.as_ref().filter(|s| !s.is_empty()) {
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

        let (line, comment) = {
            let (l, c) = line.split_once(';').unzip();
            (l.unwrap_or(line), c.map(str::trim_end))
        };

        let contents = if let Ok((bracket, _)) = parse::parse_bracket_line.parse(line) {
            match bracket {
                '{' => file_state.push_context(),
                '}' => file_state.pop_context().unwrap(),
                _ => unreachable!(),
            }

            LineContent::Bracket(bracket)
        } else if let Ok(parsed) = parse::parse_fillto_line.parse(line) {
            LineContent::FillTo(FillTo {
                address: parsed.line_addr,
                target: parsed.target,
                fill_byte: parsed.fill_byte,
            })
        } else if let Ok(parsed) = parse::parse_code_line.parse(line) {
            let ParsedCodeLine {
                line_addr,
                instruction_bytes,
                mnemonic: _,
                logged_address,
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
                special_type,
            )
        } else if let Ok(parsed) = parse::parse_data_line.parse(line) {
            LineContent::Data(Data {
                address: parsed.line_addr,
                data: parsed.data_values,
                special_type,
            })
        } else if let Ok(parsed) = parse::parse_data_line_continuation.parse(line) {
            LineContent::Data(Data {
                address: file_state.cur_addr,
                data: parsed.data_values,
                special_type,
            })
        } else if line.trim().is_empty() {
            let mut result = LineContent::Empty;

            if let Some(comment) = comment {
                // Parse special comments
                if let Ok(parsed) = parse::parse_sub_comment.parse(comment) {
                    let (low_addr, description) = parsed;
                    result = LineContent::SubMarker(
                        file_state.addr_in_current_bank(low_addr),
                        description.trim().to_owned(),
                    );
                } else if let Some(rest) = comment.strip_prefix("@!") {
                    process_directive(rest, file_state).unwrap();
                }
            }

            result
        } else {
            panic!("Failed to parse line: {line}");
        };

        Line {
            contents,
            comment: comment.map(ToOwned::to_owned),
        }
    }
}

fn process_code_line(
    file_state: &mut FileParsingState,
    address: Addr,
    opcodes: &[u8],
    logged_addr: Option<(Bank, u16)>,
    special_type: Option<SpecialParsingType>,
) -> LineContent {
    let opcode = Opcode::get(opcodes[0]);
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
