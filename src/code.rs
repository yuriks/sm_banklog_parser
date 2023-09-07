use std::fmt::Write;

use crate::config::Override;
use crate::label::LabelMap;
use crate::opcode::StaticAddress;
use crate::{
    addr16_with_bank, addr_with_bank,
    config::Config,
    opcode::{AddrMode, Opcode},
    split_addr, split_addr16, Addr, Bank, InstructionPrototype,
};

fn canonicalize_bank(addr: Addr) -> Bank {
    match split_addr16(addr) {
        // WRAM mirrors
        ((0x00..=0x3F) | (0x80..=0xBF), 0x0000..=0x1FFF) => 0x7E,
        // IO mirrors
        ((0x00..=0x3F) | (0x80..=0xBF), 0x2000..=0x5FFF) => 0x00,
        (bank, _) => bank,
    }
}

#[derive(Debug, Clone)]
pub struct Code {
    pub address: Addr,
    pub opcode: &'static Opcode,
    pub raw_operand: u32,
    pub operand_size: u8,
    pub comment: Option<String>,

    pub logged_bank: Option<Bank>,

    pub instruction_prototype: Option<InstructionPrototype>,
}

impl Code {
    fn arg_label(&self, config: &Config, labels: &LabelMap) -> Option<String> {
        let operand_value = self.get_operand();
        match operand_value {
            StaticAddress::None => return None,
            StaticAddress::BlockMove { src, dst } => {
                return Some(format!("${src:02X},${dst:02X}"));
            }
            // TODO: This is kinda ew
            _ => {}
        }

        let override_ = config.get_override(self.address);
        let label_addr = self.get_operand_label_address(override_);

        let label = label_addr.and_then(|label_addr| {
            if self.opcode.addr_mode == AddrMode::PcRelative
                || self.opcode.addr_mode == AddrMode::PcRelativeLong
                || self.opcode.name == "JSR"
                || self.opcode.name == "JSL"
            {
                labels.get_label(label_addr)
            } else {
                labels.get_label_fuzzy(label_addr)
            }
        });

        let result = match label {
            Some(l) if !l.is_blocked() => {
                l.use_from(self.address);

                let offset = l.offset_to_operand(operand_value).unwrap();
                let offset = match self.operand_size {
                    // This removes the multiple of 0x1_0000 when the operand and label are in
                    // different banks (which are considered equivalent due to canonicalization)
                    1 | 2 => i32::from(offset as i16),
                    _ => offset,
                };

                if offset == 0 {
                    l.name.to_string()
                } else if offset.abs() < 10 {
                    format!("{}{:+}", l.name, offset)
                } else {
                    let sign = if offset >= 0 { '+' } else { '-' };
                    format!("{}{}${:X}", l.name, sign, offset.abs())
                }
            }
            _ => match self.operand_size {
                1 => format!("${:02X}", self.raw_operand),
                2 => format!("${:04X}", self.raw_operand),
                3 => format!("${:06X}", self.raw_operand),
                x => panic!("Invalid argument length: {x}"),
            },
        };

        Some(result)
    }

    pub fn get_operand(&self) -> StaticAddress {
        self.opcode.addr_mode.static_label_address(
            self.address,
            1 + u16::from(self.operand_size),
            self.raw_operand,
        )
    }

    pub fn estimate_operand_canonical_bank(&self) -> Option<Bank> {
        let (code_bank, _) = split_addr(self.address);
        let operand = self.get_operand();
        // TODO: Overrides
        let (bank, addr) = match operand {
            StaticAddress::None | StaticAddress::BlockMove { .. } => None,
            StaticAddress::Long(addr) => Some(split_addr16(addr)),
            StaticAddress::DataBank(low_addr) => {
                Some((self.logged_bank.unwrap_or(code_bank), low_addr))
            }
            // TODO: is this right?
            StaticAddress::Immediate(value) => Some((self.logged_bank.unwrap_or(code_bank), value)),
        }?;

        let canonical_bank = if self.opcode.addr_mode == AddrMode::AbsoluteLongIndexed
            && (0x80..=0xCF).contains(&(self.raw_operand >> 16))
            && (self.raw_operand & 0xFFFF) < 0x40
        {
            // This is sometimes used for accessing struct fields relative to a pointer to
            // another bank, which would be incorrectly canonicalized as a WRAM mirror.
            bank
        } else {
            canonicalize_bank(addr_with_bank(bank, Addr::from(addr)))
        };

        Some(canonical_bank)
    }

    pub fn get_operand_label_address(&self, override_: Option<&Override>) -> Option<Addr> {
        let (code_bank, _) = split_addr(self.address);

        // label_addr override takes precedence over any automatic logic
        if let Some(addr_override) = override_.and_then(|o| o.label_addr) {
            return Some(addr_override);
        }

        let override_db = override_.and_then(|o| o.db);
        // TODO: More complex overrides

        let operand = self.get_operand();
        let (bank, mut low_addr) = match operand {
            StaticAddress::None | StaticAddress::BlockMove { .. } => return None,
            StaticAddress::Long(addr) => split_addr16(addr),
            StaticAddress::DataBank(low_addr) => (
                override_db.or(self.logged_bank).unwrap_or(code_bank),
                low_addr,
            ),
            StaticAddress::Immediate(value) => (override_db?, value),
        };

        if override_db.is_some()
            && !matches!(
                operand,
                StaticAddress::DataBank(_) | StaticAddress::Immediate(_)
            )
        {
            eprintln!("Instruction at ${:06X} has operand DB override, but addressing mode doesn't depend on DB.", self.address);
        }

        if matches!(self.opcode.addr_mode, AddrMode::AbsoluteIndexedX | AddrMode::AbsoluteIndexedY | AddrMode::AbsoluteLongIndexed)
            //&& (0x80..=0xCF).contains(&bank)
            && low_addr < 0x40
        {
            // Indexed addressing modes with low_addr near $0000 are often being used to access
            // a pointer in the index register, either in this or another bank, and sometimes with
            // a struct offset, so don't assume they are labels.
            // TODO: Catch more overrides (for structs f.ex.)
            override_db?;
        }

        let canonical_bank = canonicalize_bank(addr16_with_bank(bank, low_addr));

        if self.opcode.name == "PEA" {
            if (low_addr & 0x00FF) == 0 || (low_addr & 0xFF) == (low_addr >> 8) {
                // PEA of the form $db00 or $dbdb are usually used as part of a
                // PEA : PLB : PLB sequence to change bank, rather than as a
                // code label.
                // TODO: Make sure this will work for other overrides
                override_db?;
            } else {
                // Otherwise, it's probably being used to push an address for
                // an RTS to return to. RTS increments the popped address, so
                // adjust the created label so it's correctly placed.
                low_addr += 1;
            }
        }

        Some(addr16_with_bank(canonical_bank, low_addr))
    }

    //noinspection IncorrectFormatting
    #[allow(clippy::match_same_arms)]
    pub fn to_string(&self, config: &Config, labels: &LabelMap) -> String {
        let arg_label = self.arg_label(config, labels).unwrap_or_default();
        #[rustfmt::skip]
        let opcode = match self.opcode.addr_mode {
            AddrMode::Absolute | AddrMode::CodeAbsolute => format!("{}.w {}", self.opcode.name, arg_label),
            AddrMode::AbsoluteIndexedIndirect =>        format!("{}.w ({},X)", self.opcode.name, arg_label),
            AddrMode::AbsoluteLongIndexed =>            format!("{}.l {},X", self.opcode.name, arg_label),
            AddrMode::AbsoluteIndexedX =>               format!("{}.w {},X", self.opcode.name, arg_label),
            AddrMode::AbsoluteIndexedY =>               format!("{}.w {},Y", self.opcode.name, arg_label),
            AddrMode::AbsoluteIndirect =>               format!("{}.w ({})", self.opcode.name, arg_label),
            AddrMode::AbsoluteIndirectLong =>           format!("{}.w [{}]", self.opcode.name, arg_label),
            AddrMode::AbsoluteLong =>                   format!("{}.l {}", self.opcode.name, arg_label),
            AddrMode::BlockMove =>                      format!("{} {}", self.opcode.name, arg_label),
            AddrMode::Direct =>                         format!("{}.b {}", self.opcode.name, arg_label),
            AddrMode::DirectIndexedIndirect =>          format!("{}.b ({},X)", self.opcode.name, arg_label),
            AddrMode::DirectIndexedX =>                 format!("{}.b {},X", self.opcode.name, arg_label),
            AddrMode::DirectIndexedY =>                 format!("{}.b {},Y", self.opcode.name, arg_label),
            AddrMode::DirectIndirect =>                 format!("{}.b ({})", self.opcode.name, arg_label),
            AddrMode::DirectIndirectIndexed =>          format!("{}.b ({}),Y", self.opcode.name, arg_label),
            AddrMode::DirectIndirectLongIndexed =>      format!("{}.b [{}],Y", self.opcode.name, arg_label),
            AddrMode::DirectIndirectLong =>             format!("{}.b [{}]", self.opcode.name, arg_label),
            AddrMode::Immediate =>                      format!("{}.{} #{}", self.opcode.name, if self.operand_size == 1 { "b" } else { "w" }, arg_label),
            AddrMode::ImmediateByte =>                  format!("{}.b #{}", self.opcode.name, arg_label),
            AddrMode::Implied =>                        self.opcode.name.to_string(),
            AddrMode::PcRelative =>                     format!("{} {}", self.opcode.name, arg_label),
            AddrMode::PcRelativeLong =>                 format!("{} {}", self.opcode.name, arg_label),
            AddrMode::StackRelative =>                  format!("{}.b {},S", self.opcode.name, arg_label),
            AddrMode::StackRelativeIndirectIndexed =>   format!("{}.b ({},S),Y", self.opcode.name, arg_label),
        };

        let mut result = format!("    {:<39} ; ${:06X} |", opcode, self.address);
        if let Some(comment) = &self.comment {
            write!(&mut result, " {comment}").unwrap();
        }

        result
    }
}
