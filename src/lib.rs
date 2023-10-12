#![warn(clippy::pedantic)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::items_after_statements)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::similar_names)]
#![allow(clippy::verbose_bit_mask)]

use std::collections::BTreeMap;
use std::fmt::Write;
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Write as _};
use std::mem;
use std::str::FromStr;

use anyhow::{anyhow, Context};
use glob::glob;
use regex::Regex;
use winnow::Parser;

use crate::code::Code;
use crate::config::Config;
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
mod types;

pub use types::{Addr, Bank};

pub(crate) fn split_addr16(addr: Addr) -> (Bank, u16) {
    (Bank::of(addr), (addr & 0xFFFF) as u16)
}

fn is_bulk_data(addr: Addr) -> bool {
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

fn should_skip_bulk_data(line_addr: Addr, line: &Line, currently_skipping: bool) -> bool {
    let is_ascii_art_comment = |s| parse::recognize_ascii_art_comment.parse(s).is_ok();

    match line {
        Line {
            contents: LineContent::Data(..),
            ..
        } if is_bulk_data(line_addr) => true,

        Line {
            contents: LineContent::Empty,
            comment: None,
            ..
        } if currently_skipping => true,

        Line {
            comment: Some(c), ..
        } if is_bulk_data(line_addr) && is_ascii_art_comment(c) => true,

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

    cur_addr: Addr,
}

impl FileParsingState {
    fn new(start_addr: Addr) -> Self {
        FileParsingState {
            modifier_stack: vec![ParsingModifiers::default()],
            prefixed_instruction_directive: None,
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

fn parse_files() -> Banks {
    let mut result = BTreeMap::new();

    let filenames = glob("./logs/*.asm").unwrap();
    let filename_regex = Regex::new(r"Bank (\$[0-9A-F]{2})(?:\.\.(\$[0-9A-F]{2}))?").unwrap();
    for filename in filenames.flatten() {
        let cap = filename_regex.captures(filename.to_str().unwrap()).unwrap();
        let bank_start = Bank::from_str(&cap[1]).unwrap();
        let bank_end = cap
            .get(2)
            .map_or(bank_start, |m| Bank::from_str(m.as_str()).unwrap());

        let file = BufReader::new(File::open(&filename).unwrap());
        let mut file_state = FileParsingState::new(bank_start.addr(0x8000));
        let mut lines = Vec::new();
        let mut running_bulk_bytes_omitted = 0;

        let emit_bulk_data_summary = |bytes_omitted: &mut Addr, lines: &mut Vec<Line>| {
            if *bytes_omitted > 0 {
                lines.push(Line::new_comment(format!(
                    " ==> [pjdasm] ${bytes_omitted:X} bytes omitted"
                )));
                *bytes_omitted = 0;
            }
        };

        /* Parse the full file into data */
        for line in file.lines() {
            let line = line.unwrap(); // TODO: Error handling

            let parsed = Line::parse(&line, &mut file_state);
            let line_addr = parsed.address().unwrap_or(file_state.cur_addr);
            let pc_advance = parsed.contents.pc_advance();
            file_state.cur_addr = line_addr + pc_advance;

            // Handle bank-crossing wrap around
            if split_addr16(file_state.cur_addr).1 < 0x8000 {
                file_state.cur_addr |= 0x8000;
            }

            let bank = Bank::of(line_addr);
            if (bank < bank_start || bank > bank_end) && parsed.contents.produces_output() {
                eprintln!(
                    "Line defined outside of the bank range of its file: ${:06X} in `{}`. Line: {:#X?}",
                    line_addr,
                    filename.display(),
                    parsed
                );
            }

            if should_skip_bulk_data(line_addr, &parsed, running_bulk_bytes_omitted > 0) {
                running_bulk_bytes_omitted += pc_advance;
            } else {
                emit_bulk_data_summary(&mut running_bulk_bytes_omitted, &mut lines);
                lines.push(parsed);
            }
        }
        emit_bulk_data_summary(&mut running_bulk_bytes_omitted, &mut lines);

        result.insert(bank_start, lines);
    }

    result
}

fn generate_instruction_prototype_labels(banks: &Banks, labels: &mut LabelMap) {
    for line in banks.values().flatten() {
        if let LineContent::Code(Code {
            address,
            instruction_prototype: Some(instr_proto),
            ..
        }) = &line.contents
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
    }
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
        let a0_lines = banks.get(&Bank(0xA0)).context("Bank $A0 not in input")?;
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
    let enemy_banks = (0xA2u8..=0xAA).chain(0xB2..=0xB3).map(Bank);
    for bank in enemy_banks {
        let bank_lines = banks
            .get_mut(&bank)
            .with_context(|| format!("Bank {bank} doesn't exist"))?;

        let insertion_pos = 1 + bank_lines
            .iter()
            .position(|l| {
                l.comment
                    .as_ref()
                    .is_some_and(|c| c.trim_start().starts_with("See bank $A0"))
            })
            .with_context(|| format!("Target line not found in bank {bank}"))?;

        bank_lines.splice(
            insertion_pos..insertion_pos,
            template_lines.iter().cloned().map(|line| {
                let Some(address) = line.address() else {
                    return line;
                };

                let new_addr = bank.addr(address as u16);
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
                        if let Some(Bank(0xA0)) = c.get_operand_label_address(None).map(Bank::of) {
                            overrides.add_override(Override {
                                db: Some(Bank(0xA0)),
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

struct DisplayLine<'l> {
    prefix_lines: String,
    address: Option<Addr>,
    line: String,
    comment: Option<&'l str>,
}

fn emit_bank_lines<'l>(
    bank_lines: &'l Vec<Line>,
    overrides: &OverrideMap,
    labels: &LabelMap,
    mut cb: impl FnMut(DisplayLine<'l>, &Line) -> anyhow::Result<()>,
) -> anyhow::Result<()> {
    let mut current_addr = Addr::MAX;

    for line in bank_lines {
        let address = line.address();
        let mut prefix_lines = String::new();

        if let Some(address) = address {
            if address != current_addr {
                current_addr = address;
                writeln!(&mut prefix_lines, "org ${current_addr:06X}").unwrap();
            }

            let pc_advance = line.contents.pc_advance();
            if pc_advance != 0 {
                let label = labels.get_label_exact(address);
                if let Some(label) = label {
                    writeln!(&mut prefix_lines, "{}:", label.name()).unwrap();
                    label.assigned.set(true);
                }
                current_addr += pc_advance;
            }
        }

        cb(
            DisplayLine {
                prefix_lines,
                address,
                line: line.to_string(overrides, labels),
                comment: line.comment.as_deref(),
            },
            line,
        )?;
    }

    Ok(())
}

fn write_output_files(
    banks: &Banks,
    overrides: &OverrideMap,
    labels: &LabelMap,
) -> anyhow::Result<()> {
    fn create_output_file(path: &str) -> anyhow::Result<BufWriter<File>> {
        Ok(BufWriter::new(File::create(format!("./asm/{path}"))?))
    }

    let mut main_file = create_output_file("main.asm")?;
    writeln!(main_file, "lorom")?;
    writeln!(main_file, "incsrc labels.asm")?;

    for (bank, bank_lines) in banks {
        let mut output_file = create_output_file(&format!("bank_{bank:02x}.asm"))?;
        writeln!(main_file, "incsrc bank_{bank:02x}.asm")?;

        emit_bank_lines(bank_lines, overrides, labels, |disp, line| {
            let add_address_to_comment =
                matches!(line.contents, LineContent::Data(_) | LineContent::Code(_));
            let has_comment = add_address_to_comment || disp.comment.is_some();

            let (first_line, remaining_lines) = disp.line.split_once('\n').unzip();
            let first_line = first_line.unwrap_or(&disp.line);

            write!(output_file, "{}", disp.prefix_lines)?;

            if has_comment {
                if first_line.is_empty() {
                    write!(output_file, ";")?;
                } else {
                    write!(output_file, "{:<43} ;", first_line)?;
                }

                if let Some(address) = disp.address.filter(|_| add_address_to_comment) {
                    let (bank, low_addr) = split_addr16(address);
                    write!(output_file, " ${bank:02X}:{low_addr:04X} ;")?;
                }
                if let Some(comment) = disp.comment.as_ref().filter(|s| !s.is_empty()) {
                    write!(output_file, "{}", comment)?;
                }
                writeln!(output_file)?;
            } else {
                writeln!(output_file, "{}", first_line)?;
            }

            if let Some(rest) = remaining_lines {
                writeln!(output_file, "{}", rest)?;
            }

            Ok(())
        })?;
    }

    fn dump_labels(
        labels: &LabelMap,
        filename: &str,
        pred: impl FnMut(&&Label) -> bool,
    ) -> anyhow::Result<()> {
        let mut output_file = create_output_file(filename)?;
        writeln!(output_file, "; Generated by pjdasm")?;
        for l in labels.iter_labels().filter(pred) {
            writeln!(output_file, "{} = ${:06X}", l.name(), l.address)?;
        }

        Ok(())
    }

    dump_labels(labels, "labels.asm", |l| {
        !l.assigned.get() && !l.is_blocked()
    })?;
    dump_labels(labels, "all_labels.asm", |l| {
        l.assigned.get() && !l.is_blocked()
    })?;
    dump_labels(labels, "externals.asm", |l| {
        l.assigned.get() && l.is_external() && !l.is_blocked()
    })?;

    Ok(())
}

pub fn run() {
    let mut config = Config::load("./config/").expect("Failed to read config");
    let mut overrides = OverrideMap::from_config(mem::take(&mut config.overrides));
    let mut labels = LabelMap::from_config(mem::take(&mut config.labels));

    println!("Parsing...");
    let mut banks = parse_files();
    generate_instruction_prototype_labels(&banks, &mut labels);

    println!("Indexing...");
    clone_shared_enemy_ai_library(&mut banks, &mut overrides, &mut labels).unwrap();
    label::generate_overrides(&mut overrides, &labels);
    structs::generate_overrides(&mut overrides, &config, &labels);
    label::generate_labels(&mut labels, &banks, &overrides);

    println!("Generating...");
    write_output_files(&banks, &overrides, &labels).unwrap();
}
