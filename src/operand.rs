use std::collections::BTreeMap;
use std::fmt;
use std::fmt::{Debug, Display};

use serde::Deserialize;

use crate::label::Label;
use crate::{Addr, Bank};

#[derive(Copy, Clone, Debug, PartialEq, Deserialize)]
#[serde(untagged)]
pub enum OverrideAddr {
    Address(Addr),
    Range(Addr, Addr),
}

#[derive(Copy, Clone, Debug, PartialEq, Deserialize)]
pub enum OperandType {
    Literal,
    Address,
    /// Address which will be labelled even if it's value is 0
    NonNullAddress,
}

#[derive(Clone, PartialEq, Deserialize)]
pub struct Override {
    pub addr: OverrideAddr,

    /// Label at this address will be used as the label (with an offset if necessary)
    pub label_addr: Option<Addr>,
    /// Specifies a bank which will be used to lookup short addresses. Otherwise falls back to a
    /// logged bank, or the same as a program bank. If set on an immediate, then it'll be assumed
    /// to be a short pointer to this bank when looking up a label.
    pub db: Option<Bank>,

    /// Determines if the operand value will be emitted as a numeric literal, or as a reference to
    /// an appropriate label. If `db` or `label_addr` are set, this defaults to [`Address`],
    /// otherwise:
    /// - For code locations, immediates default to [`Literal`] and memory references default to
    ///   [`Address`].
    /// - For data locations, `dl` defaults to [`Address`], otherwise defaults to [`Literal`].
    ///
    /// [`Address`]: OperandType::Address
    /// [`Literal`]: OperandType::Literal
    #[serde(rename = "type")]
    pub operand_type: Option<OperandType>,

    /// Labels auto-generated from this operand will use this as the label name prefix.
    pub label_name_hint: Option<String>,

    #[serde(rename = "struct")]
    pub struct_: Option<String>,
}

impl Override {
    pub fn new(addr: OverrideAddr) -> Self {
        Self {
            addr,
            label_addr: None,
            db: None,
            operand_type: None,
            label_name_hint: None,
            struct_: None,
        }
    }
}

impl Debug for Override {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &Self {
            addr,
            label_addr,
            db,
            operand_type,
            label_name_hint,
            struct_,
        } = &self;

        macro_rules! fmt_field {
            ($field:ident) => {
                if let Some($field) = $field {
                    write!(f, concat!(stringify!($field), ": {:#X?}, "), $field)?;
                }
            };
        }

        write!(f, "Override {{ addr: {addr}, ")?;
        fmt_field!(label_addr);
        fmt_field!(db);
        fmt_field!(operand_type);
        fmt_field!(label_name_hint);
        fmt_field!(struct_);
        write!(f, "}}")
    }
}

impl OverrideAddr {
    fn as_range(self) -> (Addr, Addr) {
        match self {
            OverrideAddr::Address(x) => (x, x),
            OverrideAddr::Range(a, b) => (a, b),
        }
    }

    fn overlaps(&self, o: &OverrideAddr) -> bool {
        let (start1, end1) = self.as_range();
        let (start2, end2) = o.as_range();

        start1 <= end2 && end1 >= start2
    }

    pub fn first(self) -> Addr {
        self.as_range().0
    }

    fn last(self) -> Addr {
        self.as_range().1
    }

    pub fn contains(self, addr: Addr) -> bool {
        match self {
            OverrideAddr::Address(x) => addr == x,
            OverrideAddr::Range(a, b) => addr >= a && addr <= b,
        }
    }
}

impl Display for OverrideAddr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OverrideAddr::Address(addr) => {
                write!(f, "${addr:06X}")
            }
            OverrideAddr::Range(range_begin, range_end) => {
                write!(f, "${range_begin:06X}..${range_end:06X}")
            }
        }
    }
}

#[derive(Clone)]
pub struct OverrideMap {
    overrides: BTreeMap<Addr, Override>,
}

impl OverrideMap {
    pub fn from_config(config: Vec<Override>) -> OverrideMap {
        // TODO: Check overrides for overlaps
        OverrideMap {
            overrides: config.into_iter().map(|o| (o.addr.first(), o)).collect(),
        }
    }

    pub fn get_override(&self, addr: Addr) -> Option<&Override> {
        let (_, ov) = self.overrides.range(..=addr).last()?;
        if ov.addr.contains(addr) {
            Some(ov)
        } else {
            None
        }
    }

    pub fn add_override(&mut self, override_: Override) {
        // Check for an overlapping override by checking insertion neighbors
        let preceding_ov = self
            .overrides
            .range(..=override_.addr.first())
            .last()
            .map(|(_, v)| v);
        let following_ov = self
            .overrides
            .range(override_.addr.last()..)
            .next()
            .map(|(_, v)| v);
        for neighbor in itertools::chain(preceding_ov, following_ov) {
            if override_.addr.overlaps(&neighbor.addr) {
                eprintln!("Overlapping overrides: {override_:?} and {neighbor:?}");
                return;
            }
        }

        self.overrides.insert(override_.addr.first(), override_);
    }
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
                #[allow(unused_variables)] // RustRover false positive
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
