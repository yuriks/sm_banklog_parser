use std::cell::Cell;
use std::collections::BTreeMap;
use std::fmt;
use std::ops::Deref;

use serde::Deserialize;

use crate::code::Code;
use crate::config::{OperandType, Override, OverrideType};
use crate::data::{get_data_label_address, Data};
use crate::directives::InstructionPrototype;
use crate::{addr_with_bank, config::Config, line::Line, opcode::AddrMode, split_addr, Addr, Bank};

pub struct LabelMap(pub(crate) BTreeMap<Addr, Label>);
//pub type LabelMap = BTreeMap<u64, Label>;

impl Deref for LabelMap {
    type Target = BTreeMap<Addr, Label>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl LabelMap {
    pub fn new() -> Self {
        LabelMap(BTreeMap::new())
    }

    pub fn get_label(&self, label_addr: Addr) -> Option<&Label> {
        self.0.get(&label_addr)
    }

    pub fn get_label_fuzzy(&self, label_addr: Addr) -> Option<&Label> {
        for offset in [0, -1, 1] {
            if let Some(label) = self.0.get(&label_addr.wrapping_add_signed(offset)) {
                return Some(label);
            }
        }
        None
    }
}

#[derive(Clone, Debug, Default, PartialEq, Deserialize)]
#[serde(tag = "type")]
pub enum LabelType {
    #[default]
    Undefined,
    // Subroutine that returns with RTS
    Subroutine,
    // Subroutine that returns with RTL
    SubroutineLong,
    #[serde(skip)]
    Instruction(InstructionPrototype),
    Branch,
    Data,
    PointerTable {
        length: u64,
    },
    DataTable {
        length: u64,
    },
    Blocked,
}

#[derive(Debug)]
pub struct Label {
    pub address: Addr,
    pub name: String,
    pub label_type: LabelType,
    pub assigned: Cell<bool>,
    external: Cell<bool>,
}

impl Label {
    pub fn new(address: Addr, name: String, label_type: LabelType) -> Self {
        Label {
            address,
            name,
            label_type,
            assigned: false.into(),
            external: false.into(),
        }
    }

    pub fn use_from(&self, use_addr: Addr) {
        if use_addr >> 16 != self.address >> 16 {
            self.external.set(true);
        }
    }

    pub fn attempt_use(&self, usage_site: Addr) -> Option<&Label> {
        if self.is_blocked() {
            None
        } else {
            self.use_from(usage_site);
            Some(self)
        }
    }

    pub fn is_external(&self) -> bool {
        self.external.get()
    }

    pub fn is_blocked(&self) -> bool {
        matches!(self.label_type, LabelType::Blocked)
    }
}

#[rustfmt::skip]
pub fn generate_labels(lines: &BTreeMap<Addr, Vec<Line>>, config: &Config, labels: &mut LabelMap) {
    /* Pre-initialize all labels from the config file */
    for label in &config.labels {
        labels.0.entry(label.addr).or_insert(Label::new(
            label.addr,
            label.name.clone(),
            label.type_.clone().unwrap_or_default()));
    }

    for addr_lines in lines.values() {
        for line in addr_lines {
            match line {
                Line::Code(c) => {
                    if let Some(label) = generate_label_for_line_operand(config, c) {
                        match label.label_type {
                            LabelType::DataTable { .. } | LabelType::PointerTable { .. } => {
                                if labels.get_label_fuzzy(label.address).is_none() {
                                    labels.0.insert(label.address, label);
                                }
                            },
                            _ => {
                                labels.0.entry(label.address).or_insert(label);
                            },
                        }
                    }
                },
                Line::Data(data) => generate_labels_for_line_data(config, labels, data),
                Line::Comment(_) => {}
            }
        }
    }
}

fn generate_label_for_line_operand(config: &Config, c: &Code) -> Option<Label> {
    let override_ = config.get_override(c.address);

    let label_addr = c.get_operand_label_address(override_)?;

    let default_label_type = match (c.opcode.name, c.opcode.addr_mode) {
        // PEA operand detected as immediate will return early from get_operand_label_address above
        ("PEA", _) | ("JSR", AddrMode::CodeAbsolute) => LabelType::Subroutine,
        ("JSL", _) => LabelType::SubroutineLong,
        (_, AddrMode::AbsoluteIndexedIndirect) => LabelType::PointerTable { length: 0 },
        (
            _,
            AddrMode::AbsoluteLongIndexed | AddrMode::AbsoluteIndexedX | AddrMode::AbsoluteIndexedY,
        ) => LabelType::DataTable { length: 0 },
        (_, AddrMode::PcRelative) => LabelType::Branch,
        (_, AddrMode::Absolute | AddrMode::CodeAbsolute | AddrMode::AbsoluteLong) => {
            LabelType::Data
        }
        _ => LabelType::Undefined,
    };

    generate_automatic_label(label_addr, override_, default_label_type)
}

/// Scan through data and insert labels for data pointers (from overrides)
fn generate_labels_for_line_data(config: &Config, labels: &mut LabelMap, data: &Data) {
    let mut cur_pc = data.address;
    for d in &data.data {
        let override_ = config.get_override(cur_pc);

        if let Some(OverrideType::Struct) = override_.and_then(|o| o.type_) {
            // TODO: This code needs to be unified with the other side of the branch when overrides/labels are overhauled
            /* Handle struct overrides */
            let ov_struct = override_.and_then(|o| o.struct_.as_ref());
            if let Some(st) =
                ov_struct.and_then(|ov_struct| config.structs.iter().find(|s| s.name == *ov_struct))
            {
                let cur_offset = cur_pc - data.address;
                let cur_st_offset = cur_offset % st.size();
                let field = st.field_at_offset(cur_st_offset).unwrap();
                if field.type_ == OverrideType::Pointer {
                    let db = field.db.unwrap_or((cur_pc >> 16) as Bank);
                    let label_addr = if field.length < 3 {
                        addr_with_bank(db, d.as_u64())
                    } else {
                        d.as_u64()
                    };
                    if (label_addr & 0xFFFF) >= 0x8000 {
                        labels.0.entry(label_addr).or_insert(Label::new(
                            label_addr,
                            format!("{}_{:04X}", field.name, label_addr & 0xFFFF_u64),
                            LabelType::Subroutine,
                        ));
                    }
                }
                if cur_st_offset == 0 {
                    labels
                        .0
                        .entry(cur_pc)
                        .and_modify(|l| l.name = format!("{}_{:04X}", st.name, cur_pc & 0xFFFF_u64))
                        .or_insert(Label::new(
                            cur_pc,
                            format!("{}_{:04X}", st.name, cur_pc & 0xFFFF_u64),
                            LabelType::DataTable { length: 0 },
                        ));
                }
            }
        } else {
            let (type_, label_addr) =
                get_data_label_address(config, data.address, cur_pc, *d, override_);

            if type_ == OperandType::Address {
                let label = generate_automatic_label(label_addr, override_, LabelType::Undefined);
                if let Some(label) = label {
                    labels.0.entry(label_addr).or_insert(label);
                }
            }
        }

        cur_pc += d.length();
    }
}

fn generate_automatic_label(
    label_addr: Addr,
    override_: Option<&Override>,
    default_label_type: LabelType,
) -> Option<Label> {
    let (label_bank, label_low_addr) = split_addr(label_addr);
    let region_str = match (label_bank, label_low_addr) {
        (0x7E, 0x00..=0xFF) => return None, // Don't label DP for now
        (0x7E, 0x100..=0x1FFF) => "LORAM",
        (0x7E..=0x7F, _) => "WRAM",
        (0x00, 0x2000..=0x7FFF) => "HWREG",
        (0x70..=0x7D, _) => "SRAM",
        _ => "",
    };

    let override_type = override_.and_then(|o| o.type_).map(|ot| match ot {
        OverrideType::Pointer => LabelType::PointerTable { length: 0 },
        OverrideType::Data => LabelType::Data,
        OverrideType::Struct => LabelType::Undefined, // TODO
        OverrideType::PointerTable => LabelType::PointerTable { length: 0 },
        OverrideType::DataTable => LabelType::DataTable { length: 0 },
        OverrideType::Subroutine => LabelType::Subroutine,
    });

    let usage_type = override_type.unwrap_or(default_label_type);
    let usage_str = match usage_type {
        LabelType::Undefined => "UNK",
        LabelType::Subroutine => "SUB",
        LabelType::SubroutineLong => "SUBL",
        LabelType::Instruction(_) => "INSTR",
        LabelType::Branch => "BRA",
        LabelType::Data => "DAT",
        LabelType::PointerTable { .. } => "PTR",
        LabelType::DataTable { .. } => "TBL",
        LabelType::Blocked => unreachable!(
            "Tried to create string for Blocked label @ ${:06X}",
            label_addr
        ),
    };

    let label_name = if region_str.is_empty() {
        format!("{usage_str}_{label_addr:06X}")
    } else if usage_str == "DAT" {
        format!("{region_str}_{label_addr:06X}")
    } else {
        format!("{region_str}_{usage_str}_{label_addr:06X}")
    };

    Some(Label::new(label_addr, label_name, usage_type))
}

pub enum LabelOrLiteral<'a> {
    Label(&'a Label),
    Literal(Addr),
}

pub fn format_address_expression_str(
    target: Addr,
    base: Option<LabelOrLiteral>,
    operand_size: u8,
) -> String {
    let mut result = String::new();
    format_address_expression(&mut result, target, base, operand_size).unwrap();
    result
}

pub fn format_address_expression(
    result: &mut impl fmt::Write,
    target: Addr,
    base: Option<LabelOrLiteral>,
    operand_size: u8,
) -> fmt::Result {
    let mut remaining_offset = target as i32;

    let base = base.unwrap_or(LabelOrLiteral::Literal(target));

    match base {
        LabelOrLiteral::Label(l) => {
            write!(result, "{}", l.name)?;
            remaining_offset -= l.address as i32;
        }
        LabelOrLiteral::Literal(v) => {
            let v = if operand_size <= 2 { v & 0xFFFF } else { v };
            match operand_size {
                1 => write!(result, "${v:02X}")?,
                2 => write!(result, "${v:04X}")?,
                3 => write!(result, "${v:06X}")?,
                x => panic!("Invalid argument length: {x}"),
            }
            remaining_offset -= v as i32;
        }
    }

    // TODO: Struct field access

    if operand_size <= 2 {
        // Remove the multiples of 0x1_0000 when the operand and label are in different banks.
        remaining_offset = i32::from(remaining_offset as i16);
    }
    if remaining_offset == 0 {
    } else if remaining_offset.abs() < 10 {
        write!(result, "{remaining_offset:+}")?;
    } else {
        let sign = if remaining_offset >= 0 { '+' } else { '-' };
        write!(result, "{sign}${:X}", remaining_offset.abs())?;
    }

    Ok(())
}
