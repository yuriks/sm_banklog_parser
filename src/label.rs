use std::cell::Cell;
use std::collections::BTreeMap;
use std::ops::Deref;

use serde::Deserialize;

use crate::code::Code;
use crate::config::OverrideType;
use crate::directives::InstructionPrototype;
use crate::opcode::StaticAddress;
use crate::{
    addr_with_bank,
    config::Config,
    line::Line,
    opcode::{AddrMode, Opcode},
    split_addr, Addr, Bank,
};

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

    pub fn get_label_fuzzy(&self, label_addr: Addr) -> Option<(&Label, i64)> {
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

    pub fn is_external(&self) -> bool {
        self.external.get()
    }

    pub fn is_blocked(&self) -> bool {
        matches!(self.label_type, LabelType::Blocked)
    }
}

pub fn canonicalize_bank(addr: Addr) -> Bank {
    match ((addr >> 16) as Bank, addr & 0xFFFF) {
        // WRAM mirrors
        ((0x00..=0x3F) | (0x80..=0xBF), 0x0000..=0x1FFF) => 0x7E,
        // IO mirrors
        ((0x00..=0x3F) | (0x80..=0xBF), 0x2000..=0x5FFF) => 0x00,
        (bank, _) => bank,
    }
}

pub fn canonicalize_addr(addr: Addr) -> Addr {
    addr_with_bank(canonicalize_bank(addr), addr)
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

    for line in lines.values() {
        for addr_line in line {
            let label = match addr_line {
                Line::Code(c) => generate_label_for_line_operands(config, c),
                Line::Data(data) => {
                    /* Scan through data and insert labels for data pointers (from overrides) */
                    let mut cur_pc = data.address;
                    for d in &data.data {
                        if let Some(ov) = config.get_override(cur_pc) {
                            match ov.type_ {
                                /* Handle regular pointer overrides */
                                Some(t @ (OverrideType::Pointer | OverrideType::PointerTable | OverrideType::DataTable | OverrideType::Data | OverrideType::Subroutine)) => {
                                    let db = ov.db.map_or(cur_pc >> 16, Addr::from);
                                    let label_addr = (d.as_u64() & 0xFFFF_u64) | (db << 16);
                                    let (name, label_type) = match t {
                                        OverrideType::Pointer | OverrideType::Subroutine => (format!("SUB_{label_addr:06X}"), LabelType::Subroutine),
                                        OverrideType::PointerTable => (format!("PTR_{label_addr:06X}"), LabelType::PointerTable { length: 0 }),
                                        OverrideType::DataTable => (format!("TBL_{label_addr:06X}"), LabelType::DataTable { length: 0 }),
                                        _ => (format!("DAT_{label_addr:06X}"), LabelType::Data),
                                    };

                                    labels.0.entry(label_addr).or_insert(
                                        Label::new(label_addr, name, label_type));
                                },

                                /* Handle struct overrides */
                                Some(OverrideType::Struct) => {
                                    if let Some(st) = config.structs.iter().find(|s| &s.name == ov.struct_.as_ref().unwrap_or(&String::new())) {
                                        let last_field = &st.fields[st.fields.len() - 1];
                                        let st_len = last_field.offset + last_field.length;
                                        let cur_offset = cur_pc - data.address;
                                        let cur_st_offset = cur_offset % st_len;
                                        let field = &st.fields.iter().find(|f| f.offset == cur_st_offset).unwrap();
                                        if field.type_ == OverrideType::Pointer {
                                            let db = field.db.unwrap_or((cur_pc >> 16) as Bank);
                                            let label_addr = if field.length < 3 { addr_with_bank(db, d.as_u64()) } else { d.as_u64() };
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
                                },

                                None => {},
                            }
                        }

                        cur_pc += d.length();
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

fn generate_label_for_line_operands(config: &Config, c: &Code) -> Option<Label> {
    let mut override_db = None;
    let override_ = config.get_override(c.address);
    if let Some(override_) = override_ {
        override_db = override_.db;
    }

    let label_addr = c.get_operand();
    if override_db.is_some()
        && !matches!(
            label_addr,
            StaticAddress::DataBank(_) | StaticAddress::Immediate(_)
        )
    {
        eprintln!(
            "Instruction at ${:06X} has operand DB override (for {}), but addressing mode doesn't depend on DB.",
            c.address, override_.unwrap().addr);
    }
    let label_addr = match label_addr {
        StaticAddress::None | StaticAddress::BlockMove { .. } => None,
        StaticAddress::Long(addr) => Some(addr),
        StaticAddress::DataBank(addr) => {
            let db = c.estimate_operand_canonical_bank().unwrap_or(0xFF);
            Some((Addr::from(override_db.unwrap_or(db)) << 16) + Addr::from(addr))
        }
        StaticAddress::Immediate(imm) => {
            override_db.map(|label_bank| (Addr::from(label_bank) << 16) + Addr::from(imm))
        }
    };
    let label_addr = canonicalize_addr(label_addr?);
    let (label_bank, label_low_addr) = split_addr(label_addr);

    match c.opcode {
        Opcode {
            name: "JSR",
            addr_mode: AddrMode::CodeAbsolute,
            ..
        }
        | Opcode { name: "JSL", .. } => Some(Label::new(
            label_addr,
            format!(
                "SUB{}_{:06X}",
                if c.opcode.name == "JSL" { "L" } else { "" },
                label_addr
            ),
            LabelType::Subroutine,
        )),

        Opcode {
            addr_mode: AddrMode::AbsoluteIndexedIndirect,
            ..
        } => {
            /* Anything using this is using a table of pointers (generally) */
            let prefix = match (label_bank, label_low_addr) {
                (0x7E, 0x00..=0xFF) => "", // Don't label DP for now
                (0x7E, 0x100..=0x1FFF) => "LORAM_PTR",
                (0x7E | 0x7F, _) => "WRAM_PTR",
                (0x00..=0x3F | 0x80..=0xBF, 0x2000..=0x7FFF) => "HW_PTR",
                (0x70..=0x7D, _) => "SRAM_PTR",
                _ => "PTR",
            };
            if prefix.is_empty() {
                None
            } else {
                Some(Label::new(
                    label_addr,
                    format!("{prefix}_{label_addr:06X}"),
                    LabelType::PointerTable { length: 0 },
                ))
            }
        }

        Opcode {
            addr_mode:
                AddrMode::AbsoluteLongIndexed | AddrMode::AbsoluteIndexedX | AddrMode::AbsoluteIndexedY,
            ..
        } if label_low_addr >= 0x0100 => {
            let prefix = match (label_bank, label_low_addr) {
                (0x7E, 0x00..=0xFF) => "", // Don't label DP for now
                (0x7E, 0x100..=0x1FFF) => "LORAM_TBL",
                (0x7E | 0x7F, _) => "WRAM_TBL",
                (0x00..=0x3F | 0x80..=0xBF, 0x2000..=0x7FFF) => "HW_TBL",
                (0x70..=0x7D, _) => "SRAM_TBL",
                _ => "TBL",
            };
            if prefix.is_empty() {
                None
            } else {
                Some(Label::new(
                    label_addr,
                    format!("{prefix}_{label_addr:06X}"),
                    LabelType::DataTable { length: 0 },
                ))
            }
        }

        Opcode {
            addr_mode: AddrMode::Immediate,
            ..
        } => {
            /* For now, only do this with overrides */
            if let Some(ov) = override_ {
                if let Some(
                    ov_type @ (OverrideType::DataTable
                    | OverrideType::PointerTable
                    | OverrideType::Pointer
                    | OverrideType::Data),
                ) = ov.type_
                {
                    let (name, label_type) = match ov_type {
                        OverrideType::DataTable => (
                            format!("TBL_{label_addr:06X}"),
                            LabelType::DataTable { length: 0 },
                        ),
                        OverrideType::PointerTable => (
                            format!("PTR_{label_addr:06X}"),
                            LabelType::PointerTable { length: 0 },
                        ),
                        OverrideType::Pointer => (
                            format!("SUB_{label_addr:06X}"),
                            LabelType::PointerTable { length: 0 },
                        ),
                        _ => (format!("DAT_{label_addr:06X}"), LabelType::Data),
                    };
                    Some(Label::new(label_addr, name, label_type))
                } else {
                    None
                }
            } else {
                None
            }
        }

        Opcode {
            addr_mode: AddrMode::PcRelative,
            ..
        } => {
            /* Branches */
            Some(Label::new(
                label_addr,
                format!("BRA_{label_addr:06X}"),
                LabelType::Branch,
            ))
        }

        Opcode {
            addr_mode: AddrMode::Absolute | AddrMode::CodeAbsolute | AddrMode::AbsoluteLong,
            ..
        } => {
            let mut offset = 0i64;
            let prefix = match (label_bank, label_low_addr) {
                _ if c.opcode.name == "PEA" => {
                    // TODO: override support
                    if (label_low_addr & 0x00FF) == 0
                        || (label_low_addr & 0xFF) == (label_low_addr >> 8)
                    {
                        // PEA of the form $db00 or $dbdb are usually used as part of a
                        // PEA : PLB : PLB sequence to change bank, rather than as a
                        // code label.
                        ""
                    } else {
                        // Otherwise, it's probably being used to push an address for
                        // an RTS to return to. RTS increments the popped address, so
                        // adjust the created label so it's correctly placed.
                        offset = 1;
                        "SUB"
                    }
                }
                (0x7E, 0x00..=0xFF) => "", // Don't label DP for now
                (0x7E, 0x100..=0x1FFF) => "LORAM",
                (0x7E | 0x7F, _) => "WRAM",
                (0x00..=0x3F | 0x80..=0xBF, 0x2000..=0x7FFF) => "HWREG",
                (0x70..=0x7D, _) => "SRAM",
                _ => "DAT",
            };

            if prefix.is_empty() {
                None
            } else {
                let offset_addr = label_addr.wrapping_add_signed(offset);
                Some(Label::new(
                    offset_addr,
                    format!("{prefix}_{offset_addr:06X}"),
                    if prefix == "SUB" {
                        LabelType::Subroutine
                    } else {
                        LabelType::Data
                    },
                ))
            }
        }
        _ => None,
    }
}
