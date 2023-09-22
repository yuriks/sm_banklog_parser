use std::cell::Cell;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::fmt;

use serde::Deserialize;

use crate::code::Code;
use crate::config::{OperandType, Override, OverrideAddr};
use crate::data::{get_data_label_address, Data};
use crate::directives::InstructionPrototype;
use crate::{config, config::Config, line::Line, opcode::AddrMode, split_addr, split_addr16, Addr};

pub struct LabelMap {
    labels: BTreeMap<Addr, Label>,

    /// Maps Label start address => end address.
    ///
    /// NOTE: Unlike in `OverrideAddr`, the end address is exclusive.
    regions: BTreeMap<Addr, Addr>,
}

impl LabelMap {
    pub fn new() -> Self {
        LabelMap {
            labels: BTreeMap::new(),
            regions: BTreeMap::new(),
        }
    }

    pub fn iter_labels(&self) -> impl Iterator<Item = &Label> {
        self.labels.values()
    }

    pub fn get_label_exact(&self, label_addr: Addr) -> Option<&Label> {
        self.labels.get(&label_addr)
    }

    pub fn get_label_containing(&self, addr: Addr) -> Option<&Label> {
        // Get the last region starting before `addr` and therefore (since regions cannot overlap)
        // the one which would have to be containing it. There's a `BTreeMap::upper_bound` API in
        // Rust nightly, but in the meantime the same can be achieved with `range`.
        let preceding_region = self.regions.range(..=addr).last();
        let (&region_start, &region_end) = preceding_region?;
        if addr >= region_start && addr < region_end {
            // The region should always have a corresponding label, so unwrapping here is ok to
            // assert the invariant.
            let l = self.get_label_exact(region_start).unwrap();
            assert!(l.contains(addr));
            Some(l)
        } else {
            None
        }
    }

    pub fn get_label(&self, label_addr: Addr) -> Option<&Label> {
        self.get_label_exact(label_addr)
            .or_else(|| self.get_label_containing(label_addr))
    }

    pub fn get_label_fuzzy(&self, label_addr: Addr) -> Option<&Label> {
        for offset in [0, -1, 1] {
            if let Some(label) = self.labels.get(&label_addr.wrapping_add_signed(offset)) {
                return Some(label);
            }
        }
        self.get_label_containing(label_addr)
    }

    pub fn insert_label(&mut self, new_label: Label) -> Result<&Label, (Label, &Label)> {
        // This is just dropping the mut from the references
        match self.insert_label_mut(new_label) {
            Ok(l) => Ok(l),
            Err((new_label, l)) => Err((new_label, l)),
        }
    }

    fn insert_label_mut(&mut self, new_label: Label) -> Result<&mut Label, (Label, &mut Label)> {
        match self.labels.entry(new_label.address) {
            Entry::Vacant(e) => {
                let l = e.insert(new_label);
                if l.length > 0 {
                    let existing_region = self.regions.insert(l.address, l.address + l.length);
                    assert!(existing_region.is_none());
                }
                Ok(l)
            }
            Entry::Occupied(e) => Err((new_label, e.into_mut())),
        }
    }

    pub fn insert_or_update_label(&mut self, new_label: Label) {
        if let Err((new_label, l)) = self.insert_label_mut(new_label) {
            // Update label if the new one has more information than existing one
            if new_label.label_type != LabelType::Undefined
                && l.label_type == LabelType::Undefined
                && matches!(l.name, LabelName::AutoGenerated(_))
            {
                eprintln!(
                    "Replacing label {} ({:?}) with {} ({:?})",
                    l.name(),
                    &l.label_type,
                    new_label.name(),
                    &new_label.label_type
                );
                l.label_type = new_label.label_type;
                l.name = new_label.name;
            }
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Deserialize)]
#[serde(tag = "type")]
pub enum LabelType {
    /// Unknown label type
    #[default]
    Undefined,
    /// Pseudo-label which prevents any references to it from becoming labels
    Blocked,

    /// Subroutine that returns with RTS
    Subroutine,
    /// Subroutine that returns with RTL
    SubroutineLong,
    /// Target of a short branch instruction
    Branch,
    /// Generic code target not covered by above cases
    CodeLocation,

    /// Generic data
    Data,
    DataTable,
    /// Data pointing to a memory address
    Pointer,
    PointerTable,
    /// Constant struct instance, formatted according to its fields
    Struct {
        #[serde(rename = "struct")]
        struct_: String,
    },
    StructTable {
        #[serde(rename = "struct")]
        struct_: String,
    },

    // TODO: Figure out how I want to represent this
    #[serde(skip)]
    Instruction(InstructionPrototype),
}

impl LabelType {
    pub fn is_table(&self) -> bool {
        matches!(
            self,
            LabelType::DataTable | LabelType::PointerTable | LabelType::StructTable { .. }
        )
    }
}

#[derive(Debug)]
pub enum LabelName {
    Custom(String),
    AutoGenerated(String),
}

#[derive(Debug)]
pub struct Label {
    pub address: Addr,
    name: LabelName,
    pub label_type: LabelType,

    /// Size of data covered by this label. Can be 0, which denotes a location-only label. Labels
    /// with non-0 size may not overlap each other, however, a 0-size label may be contained by
    /// another label. (For example, branch labels inside a subroutine label.)
    pub length: u64,

    pub assigned: Cell<bool>,
    external: Cell<bool>,
}

impl Label {
    pub fn new(address: Addr, name: LabelName, label_type: LabelType, length: u64) -> Self {
        Label {
            address,
            name,
            label_type,
            length,
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

    pub fn name(&self) -> &str {
        let (LabelName::Custom(name) | LabelName::AutoGenerated(name)) = &self.name;
        name.as_str()
    }

    pub fn contains(&self, addr: Addr) -> bool {
        // This will always be false for unsized labels
        addr >= self.address && addr < self.address + self.length
    }
}

impl From<config::Label> for Label {
    fn from(l: config::Label) -> Self {
        let (address, length) = match l.addr {
            OverrideAddr::Address(x) => (x, 0),
            OverrideAddr::Range(a, b) => (a, b - a + 1),
        };

        let label_type = l.type_.unwrap_or_default();
        let name = l.name.map_or_else(
            || generate_label_name(address, &label_type, None),
            LabelName::Custom,
        );

        Label {
            address,
            name,
            label_type,
            length,
            assigned: false.into(),
            external: false.into(),
        }
    }
}

pub fn load_labels(config: &Config, labels: &mut LabelMap) {
    /* Pre-initialize all labels from the config file */
    for label in config.labels.iter().cloned() {
        labels.insert_or_update_label(label.into());
    }
}

pub fn generate_labels(lines: &BTreeMap<Addr, Vec<Line>>, config: &Config, labels: &mut LabelMap) {
    for addr_lines in lines.values() {
        for line in addr_lines {
            match line {
                Line::Code(c) => generate_label_for_line_operand(config, labels, c),
                Line::Data(data) => generate_labels_for_line_data(config, labels, data),
                Line::Comment(_) => {}
            }
        }
    }
}

/// Generate overrides from pointer labels with a length defined
pub fn generate_overrides(config: &mut Config, labels: &LabelMap) {
    let generated_overrides = labels.iter_labels().filter_map(|l| {
        let override_addr = if l.length == 0 {
            return None;
        } else {
            OverrideAddr::Range(l.address, l.address + l.length - 1)
        };

        let override_type = match &l.label_type {
            LabelType::Data | LabelType::DataTable => OperandType::Literal,
            LabelType::Pointer | LabelType::PointerTable => OperandType::Address,
            // Struct auto-overrides are handled in structs::generate_overrides
            _ => return None,
        };

        Some(Override {
            db: Some(split_addr16(l.address).0),
            operand_type: Some(override_type),
            // Address ranges are inclusive
            ..Override::new(override_addr)
        })
    });
    config.add_overrides(generated_overrides);
}

fn generate_label_for_line_operand(config: &Config, labels: &mut LabelMap, c: &Code) {
    let override_ = config.get_override(c.address);

    let Some(label_addr) = c.get_operand_label_address(override_) else {
        return;
    };

    let default_label_type = match (c.opcode.name, c.opcode.addr_mode) {
        // PEA operand detected as immediate will return early from get_operand_label_address above
        ("PEA", _) | ("JSR", AddrMode::CodeAbsolute) => LabelType::Subroutine,
        ("JSL", _) => LabelType::SubroutineLong,
        (_, AddrMode::AbsoluteIndexedIndirect) => LabelType::PointerTable,
        (
            _,
            AddrMode::AbsoluteLongIndexed | AddrMode::AbsoluteIndexedX | AddrMode::AbsoluteIndexedY,
        ) => LabelType::DataTable,
        (_, AddrMode::PcRelative) => LabelType::Branch,
        (_, AddrMode::Absolute | AddrMode::CodeAbsolute | AddrMode::AbsoluteLong) => {
            LabelType::Data
        }
        _ => LabelType::Undefined,
    };

    let label_name_hint = override_.and_then(|o| o.label_name_hint.as_deref());

    if let Some(containing_label) = labels.get_label_containing(label_addr) {
        // Don't generate new labels inside a defined table
        if containing_label.label_type.is_table() {
            return;
        }
    }

    if let Some(new_label) =
        generate_automatic_label(label_addr, default_label_type, label_name_hint)
    {
        if !new_label.label_type.is_table() || labels.get_label_fuzzy(new_label.address).is_none() {
            labels.insert_or_update_label(new_label);
        }
    }
}

/// Scan through data and insert labels for data pointers (from overrides)
fn generate_labels_for_line_data(config: &Config, labels: &mut LabelMap, data: &Data) {
    let mut cur_pc = data.address;
    for d in &data.data {
        let override_ = config.get_override(cur_pc);

        let (type_, label_addr) = get_data_label_address(cur_pc, *d, override_);

        let label_name_hint = override_.and_then(|o| o.label_name_hint.as_deref());

        if type_ == OperandType::NonNullAddress
            || (type_ == OperandType::Address && split_addr16(label_addr).1 != 0)
        {
            if let Some(containing_label) = labels.get_label_containing(label_addr) {
                // Don't generate new labels inside a defined table
                if containing_label.label_type.is_table() {
                    continue;
                }
            }

            if let Some(new_label) =
                generate_automatic_label(label_addr, LabelType::Undefined, label_name_hint)
            {
                labels.insert_or_update_label(new_label);
            }
        }

        cur_pc += d.length();
    }
}

fn generate_automatic_label(
    label_addr: Addr,
    usage_type: LabelType,
    name_prefix: Option<&str>,
) -> Option<Label> {
    if matches!(label_addr, 0x7E_0000..=0x7E_00FF) {
        // Don't auto-label if in DP for now
        None
    } else {
        let name = generate_label_name(label_addr, &usage_type, name_prefix);
        Some(Label::new(label_addr, name, usage_type, 0))
    }
}

fn generate_label_name(
    label_addr: Addr,
    usage_type: &LabelType,
    name_prefix: Option<&str>,
) -> LabelName {
    let (label_bank, label_low_addr) = split_addr(label_addr);
    let region_str = match (label_bank, label_low_addr) {
        (0x7E, 0x0000..=0x1FFF) => "LORAM",
        (0x7E..=0x7F, _) => "WRAM",
        (0x00, 0x2000..=0x7FFF) => "HWREG",
        (0x70..=0x7D, _) => "SRAM",
        _ => "",
    };

    let usage_str = if let Some(prefix) = name_prefix {
        prefix
    } else {
        match usage_type {
            LabelType::Undefined => "UNK",
            LabelType::Subroutine => "SUB",
            LabelType::SubroutineLong => "SUBL",
            LabelType::CodeLocation => "LOC",
            LabelType::Instruction(_) => "INSTR",
            LabelType::Branch => "BRA",
            LabelType::Data => "DAT",
            LabelType::Pointer => "PTR",
            LabelType::DataTable | LabelType::PointerTable => "TBL",
            LabelType::Blocked => unreachable!(
                "Tried to create string for Blocked label @ ${:06X}",
                label_addr
            ),
            LabelType::Struct { struct_ } | LabelType::StructTable { struct_ } => struct_,
        }
    };

    let label_name = if region_str.is_empty() {
        format!("{usage_str}_{label_addr:06X}")
    } else if usage_str == "DAT" {
        format!("{region_str}_{label_addr:06X}")
    } else {
        format!("{region_str}_{usage_str}_{label_addr:06X}")
    };

    LabelName::AutoGenerated(label_name)
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
            write!(result, "{}", l.name())?;
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
