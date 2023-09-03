use std::cell::Cell;
use std::collections::BTreeMap;
use std::ops::Deref;

use if_chain::if_chain;
use serde::Deserialize;

use crate::{code::ArgType, config::Config, data::DataVal, line::Line, opcode::{AddrMode, Opcode}};
use crate::config::OverrideType;
use crate::directives::InstructionPrototype;

pub struct LabelMap(pub(crate) BTreeMap<u64, Label>);
//pub type LabelMap = BTreeMap<u64, Label>;

impl Deref for LabelMap {
    type Target = BTreeMap<u64, Label>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl LabelMap {
    pub fn new() -> Self {
        LabelMap(BTreeMap::new())
    }

    pub fn get_label(&self, label_addr: u64) -> Option<&Label> {
        self.0.get(&label_addr)
    }

    pub fn get_label_fuzzy(&self, label_addr: u64) -> Option<(&Label, i64)> {
        for offset in [0, -1, 1, -2, 2] {
            if let Some(label) = self.0.get(&label_addr.wrapping_add_signed(offset)) {
                return Some((label, offset));
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
    Subroutine,
    #[serde(skip)]
    Instruction(InstructionPrototype),
    Branch,
    Data,
    PointerTable { length: u64 },
    DataTable { length: u64 },
    Blocked,
}

#[derive(Debug)]
pub struct Label {
    pub address: u64,
    pub name: String,
    pub label_type: LabelType,
    pub assigned: Cell<bool>,
    external: Cell<bool>,
}

impl Label {
    pub fn new(address: u64, name: String, label_type: LabelType) -> Self {
        Label {
            address,
            name,
            label_type,
            assigned: false.into(),
            external: false.into(),
        }
    }

    pub fn use_from(&self, use_addr: u64) {
        if use_addr >> 16 != self.address >> 16 {
            self.external.set(true);
        }
    }

    pub fn is_external(&self) -> bool {
        self.external.get()
    }

    pub fn is_blocked(&self) -> bool {
        matches!(self.label_type, LabelType::Blocked)
    }
}

pub fn generate_labels(lines: &BTreeMap<u64, Vec<Line>>, config: &Config, labels: &mut LabelMap) {
    /* Pre-initialize all labels from the config file */
    for label in &config.labels {
        labels.0.entry(label.addr).or_insert(Label::new(
            label.addr,
            label.name.clone(),
            label.type_.clone().unwrap_or_default()));
    }

    for (addr, line) in lines {
        for addr_line in line {
            let label = match addr_line {
                Line::Code(c) => match c.arg {
                    ArgType::Address(arg_addr) => match c.opcode {
                        Opcode { name: "JSR", addr_mode: AddrMode::Absolute, .. } |
                        Opcode { name: "JSL", .. } => {
                            let label_addr = if c.opcode.name == "JSR" { (addr & 0xFF_0000) | (arg_addr & 0xFFFF) } else { arg_addr };
                            Some(Label::new(
                                label_addr,
                                format!("SUB{}_{:06X}", if c.opcode.name == "JSL" { "L" } else { "" }, label_addr),
                                LabelType::Subroutine))
                        },
                        Opcode { addr_mode: AddrMode::AbsoluteIndexedIndirect, .. } => {
                            /* Anything using this is using a table of pointers (generally) */
                            let arg_addr = (u64::from(c.db) << 16) | (arg_addr & 0xFFFF);
                            let bank = arg_addr >> 16;
                            let low_addr = arg_addr & 0xFFFF_u64;
                            let (label_addr, prefix) = match low_addr {
                                0x00..=0xFF if !(0x70..=0x7F).contains(&bank) || bank == 0x7E => (0, ""), // Don't label DP for now
                                0x100..=0x1FFF if !(0x70..=0x7F).contains(&bank) || bank == 0x7E => (0x7E_0000 | (low_addr & 0xFFFF), "LORAM_PTR"),
                                0x2000..=0x7FFF if !(0x40..0x80).contains(&bank) => ((low_addr & 0xFFFF), "HW_PTR"),
                                _ if bank == 0x7E || bank == 0x7F => (arg_addr, "WRAM_PTR"),
                                _ if (0x70..0x7E).contains(&bank) => (arg_addr, "SRAM_PTR"),
                                _ => (arg_addr, "PTR")
                            };
                            if label_addr > 0 {
                                Some(Label::new(
                                    label_addr,
                                    format!("{prefix}_{label_addr:06X}"),
                                    LabelType::PointerTable { length: 0 }))
                            } else {
                                None
                            }
                        },
                        Opcode {
                            addr_mode: AddrMode::AbsoluteIndexedLong
                            | AddrMode::AbsoluteIndexedX
                            | AddrMode::AbsoluteIndexedY, ..
                        } if (arg_addr & 0xFFFF) >= 0x0100 => {
                            let arg_addr = if c.opcode.addr_mode == AddrMode::AbsoluteLong {
                                arg_addr
                            } else {
                                (u64::from(c.db) << 16) | (arg_addr & 0xFFFF)
                            };
                            let bank = arg_addr >> 16;
                            let low_addr = arg_addr & 0xFFFF_u64;
                            let (label_addr, prefix) = match low_addr {
                                0x00..=0xFF if !(0x70..=0x7F).contains(&bank) || bank == 0x7E => (0, ""), // Don't label DP for now
                                0x100..=0x1FFF if !(0x70..=0x7F).contains(&bank) || bank == 0x7E => (0x7E_0000 | (low_addr & 0xFFFF), "LORAM_TBL"),
                                0x2000..=0x7FFF if !(0x40..0x80).contains(&bank) => ((low_addr & 0xFFFF), "HW_TBL"),
                                _ if bank == 0x7E || bank == 0x7F => (arg_addr, "WRAM_TBL"),
                                _ if (0x70..0x7E).contains(&bank) => (arg_addr, "SRAM_TBL"),
                                _ => (arg_addr, "TBL")
                            };
                            if label_addr > 0 {
                                Some(Label::new(
                                    label_addr,
                                    format!("{prefix}_{label_addr:06X}"),
                                    LabelType::DataTable { length: 0 } ))
                            } else {
                                None
                            }
                        },
                        Opcode { addr_mode: AddrMode::Immediate, .. } => {
                            /* For now, only do this with overrides */
                            if let Some(ov) = config.get_override(*addr) {
                                if let Some(ov_type @ (OverrideType::DataTable | OverrideType::PointerTable | OverrideType::Pointer | OverrideType::Data)) = ov.type_ {
                                    let db = ov.db.unwrap_or(addr >> 16);
                                    let label_addr = (arg_addr & 0xFFFF_u64) | (db << 16);
                                    let (name, label_type) = match ov_type {
                                        OverrideType::DataTable => (format!("TBL_{label_addr:06X}"), LabelType::DataTable { length: 0 }),
                                        OverrideType::PointerTable => (format!("PTR_{label_addr:06X}"), LabelType::PointerTable { length: 0 }),
                                        OverrideType::Pointer => (format!("SUB_{label_addr:06X}"), LabelType::PointerTable { length: 0 }),
                                        _ => (format!("DAT_{label_addr:06X}"), LabelType::Data),
                                    };
                                    Some(Label::new(label_addr, name, label_type))
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        },
                        Opcode { addr_mode: AddrMode::Relative, .. } => {
                            /* Branches */
                            let label_addr = ((*addr as i64) + 2 + i64::from((arg_addr & 0xFF) as i8)) as u64;
                            Some(Label::new(
                                label_addr,
                                format!("BRA_{label_addr:06X}"),
                                LabelType::Branch))
                        },
                        Opcode { addr_mode: AddrMode::Absolute | AddrMode::AbsoluteLong, .. } => {
                            let arg_addr = if c.opcode.addr_mode == AddrMode::AbsoluteLong {
                                arg_addr
                            } else {
                                (u64::from(c.db) << 16) | (arg_addr & 0xFFFF)
                            };
                            let bank = arg_addr >> 16;
                            let low_addr = arg_addr & 0xFFFF_u64;
                            let (label_addr, prefix) = match low_addr {
                                0x00..=0xFF if !(0x70..=0x7F).contains(&bank) || bank == 0x7E => (0, ""), // Don't label DP for now
                                0x100..=0x1FFF if !(0x70..=0x7F).contains(&bank) || bank == 0x7E => (0x7E_0000 | (low_addr & 0xFFFF), "LORAM"),
                                0x2000..=0x7FFF if !(0x40..0x80).contains(&bank) => ((low_addr & 0xFFFF), "HWREG"),
                                _ if bank == 0x7E || bank == 0x7F => (arg_addr, "WRAM"),
                                _ if (0x70..0x7E).contains(&bank) => (arg_addr, "SRAM"),
                                _ if c.opcode.name == "PEA" => (arg_addr + 1, "SUB"),
                                _ => (arg_addr, "DAT")
                            };

                            if label_addr > 0 {
                                Some(Label::new(
                                    label_addr,
                                    format!("{prefix}_{label_addr:06X}"),
                                    if prefix == "SUB" { LabelType::Subroutine } else { LabelType::Data }))
                            } else {
                                None
                            }
                        },
                        _ => None
                    },
                    _ => None
                },
                Line::Data(data) => {
                    /* Scan through data and insert labels for data pointers (from overrides) */
                    let mut cur_pc = data.address;
                    for d in &data.data {
                        let data_len = match d {
                            DataVal::DB(_) => 1,
                            DataVal::DW(_) => 2,
                            DataVal::DL(_) => 3
                        };

                        /* Handle regular pointer overrides */
                        if_chain! {
                            if let Some(ov) = config.get_override(cur_pc);
                            if let Some(t @ (OverrideType::Pointer | OverrideType::PointerTable | OverrideType::DataTable | OverrideType::Data | OverrideType::Subroutine)) = &ov.type_;
                            then {
                                let db = ov.db.unwrap_or(cur_pc >> 16);
                                let label_addr = (d.as_u64() & 0xFFFF_u64) | (db << 16);
                                let (name, label_type) = match t {
                                    OverrideType::Pointer | OverrideType::Subroutine => (format!("SUB_{label_addr:06X}"), LabelType::Subroutine),
                                    OverrideType::PointerTable => (format!("PTR_{label_addr:06X}"), LabelType::PointerTable { length: 0 }),
                                    OverrideType::DataTable => (format!("TBL_{label_addr:06X}"), LabelType::DataTable { length: 0 }),
                                    _ => (format!("DAT_{label_addr:06X}"), LabelType::Data),
                                };

                                labels.0.entry(label_addr).or_insert(
                                    Label::new(label_addr, name, label_type));
                            }
                        }

                        /* Handle struct overrides */
                        if_chain! {
                            if let Some(ov) = config.get_override(cur_pc);
                            if let Some(OverrideType::Struct) = &ov.type_;
                            then {
                                if let Some(st) = config.structs.iter().find(|s| &s.name == ov.struct_.as_ref().unwrap_or(&String::new())) {
                                    let last_field = &st.fields[st.fields.len() - 1];
                                    let st_len = last_field.offset + last_field.length;
                                    let cur_offset = cur_pc - data.address;
                                    let cur_st_offset = cur_offset % st_len;
                                    let field = &st.fields.iter().find(|f| f.offset == cur_st_offset).unwrap();
                                    if field.type_ == OverrideType::Pointer {
                                        let db = field.db.unwrap_or(cur_pc >> 16);                                    
                                        let label_addr = if field.length < 3 { (d.as_u64() & 0xFFFF_u64) | (db << 16) } else { d.as_u64() };
                                        if (label_addr & 0xFFFF) >= 0x8000 {
                                            labels.0.entry(label_addr).or_insert(Label::new(
                                                label_addr,
                                                format!("{}_{:04X}", field.name, label_addr & 0xFFFF_u64),
                                                LabelType::Subroutine));
                                        }
                                    }
                                    if cur_st_offset == 0 {
                                        labels.0.entry(cur_pc)
                                            .and_modify(|l| l.name = format!("{}_{:04X}", st.name, cur_pc & 0xFFFF_u64))
                                            .or_insert(Label::new(
                                                cur_pc,
                                                format!("{}_{:04X}", st.name, cur_pc & 0xFFFF_u64),
                                                LabelType::DataTable { length: 0 }));
                                    }
                                }
                            }
                        }

                        cur_pc += data_len;
                    }
                    None
                }
                Line::Comment(_) => None
            };

            if let Some(label) = label {
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
        }
    }
}
