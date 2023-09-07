use std::cell::Cell;
use std::collections::BTreeMap;
use std::ops::Deref;

use serde::Deserialize;

use crate::code::Code;
use crate::config::OverrideType;
use crate::directives::InstructionPrototype;
use crate::opcode::StaticAddress;
use crate::{
    addr16_with_bank, addr_with_bank, config::Config, line::Line, opcode::AddrMode, split_addr,
    Addr, Bank,
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

    pub fn is_external(&self) -> bool {
        self.external.get()
    }

    pub fn is_blocked(&self) -> bool {
        matches!(self.label_type, LabelType::Blocked)
    }

    /// Computes the offset that needs to be added to a label to make it match the given operand.
    pub fn offset_to_operand(&self, operand: StaticAddress) -> Option<i32> {
        match operand {
            StaticAddress::None | StaticAddress::BlockMove { .. } => None,
            StaticAddress::Long(addr) => Some(addr as i32 - self.address as i32),
            StaticAddress::DataBank(low_addr) | StaticAddress::Immediate(low_addr) => {
                let addr_in_label_bank = addr16_with_bank(split_addr(self.address).0, low_addr);
                Some(addr_in_label_bank as i32 - self.address as i32)
            }
        }
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
            let label = match line {
                Line::Code(c) => generate_label_for_line_operand(config, c),
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

fn generate_label_for_line_operand(config: &Config, c: &Code) -> Option<Label> {
    let override_ = config.get_override(c.address);

    let label_addr = c.get_operand_label_address(override_)?;
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
        OverrideType::Struct => todo!(),
        OverrideType::PointerTable => LabelType::PointerTable { length: 0 },
        OverrideType::DataTable => LabelType::DataTable { length: 0 },
        OverrideType::Subroutine => LabelType::Subroutine,
    });

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
