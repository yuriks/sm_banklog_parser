use std::fmt::Write;

use crate::{Addr, addr_with_bank, Bank, config::Config, InstructionPrototype, opcode::{AddrMode, Opcode}, split_addr};
use crate::label::{canonicalize_bank, LabelMap};
use crate::opcode::{AddressingBank, StaticAddress};

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

        /* TODO: Get label if exists */
        /* Make sure to handle PC-relative addresses correctly */
        let mut override_db = None;

        let override_ = config.get_override(self.address);
        if let Some(override_) = override_ {
            override_db = override_.db;
        }

        let label_addr = operand_value;
        if override_db.is_some()
            && !matches!(
                label_addr,
                StaticAddress::DataBank(_) | StaticAddress::Immediate(_)
            )
        {
            eprintln!("Instruction at ${:06X} has operand DB override, but addressing mode doesn't depend on DB.", self.address);
        }
        let label_addr = match label_addr {
            StaticAddress::None => None,
            StaticAddress::Long(addr) => Some(addr),
            StaticAddress::DataBank(addr) => {
                let db = self.estimate_operand_canonical_bank().unwrap_or(0xFF);
                Some((Addr::from(override_db.unwrap_or(db)) << 16) + Addr::from(addr))
            }
            StaticAddress::Immediate(imm) => {
                override_db.map(|label_bank| (Addr::from(label_bank) << 16) + Addr::from(imm))
            }
            StaticAddress::BlockMove { .. } => unreachable!(),
        };

        let label = label_addr.and_then(|label_addr| {
            if self.opcode.addr_mode == AddrMode::PcRelative
                || self.opcode.addr_mode == AddrMode::PcRelativeLong
                || self.opcode.name == "JSR"
                || self.opcode.name == "JSL"
            {
                labels.get_label(label_addr).map(|l| (l, 0))
            } else if self.opcode.name == "PEA" {
                let label_low_addr = label_addr as u16;
                if (label_low_addr & 0x00FF) == 0
                    || (label_low_addr & 0xFF) == (label_low_addr >> 8)
                {
                    // TODO: Allow override
                    // PEA of the form $db00 or $dbdb are usually used as part of a
                    // PEA : PLB : PLB sequence to change bank, rather than as a
                    // code label.
                    None
                } else {
                    // Otherwise, it's probably being used to push an address for
                    // an RTS to return to. RTS increments the popped address, so
                    // adjust the created label so it's correctly placed.
                    labels
                        .get_label(label_addr.wrapping_add_signed(1))
                        .map(|l| (l, 1))
                }
            } else {
                labels.get_label_fuzzy(label_addr)
            }
        });

        let result = match label {
            Some((l, offset)) if !l.is_blocked() => {
                l.use_from(self.address);

                match offset {
                    0 => l.name.to_string(),
                    -1 | -2 => format!("{}+{}", l.name, -offset),
                    1 | 2 => format!("{}{}", l.name, -offset),
                    _ => panic!("Invalid argument length"),
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
        let label_bank = match self.opcode.addr_mode.label_bank_source() {
            AddressingBank::None => None,
            // TODO: These need to be replaced by better instruction emulation to handle PC-relative, etc.
            AddressingBank::Data => Some((self.logged_bank.unwrap_or(code_bank), self.raw_operand & 0xFFFF)),
            AddressingBank::Program => Some((code_bank, self.raw_operand & 0xFFFF)),
            AddressingBank::Direct => Some((0, self.raw_operand & 0xFFFF)),
            AddressingBank::Long => Some(((self.raw_operand >> 16) as u8, self.raw_operand & 0xFFFF)),
            AddressingBank::IndirectLong => unreachable!("IndirectLong should never be a label bank source"),
        };

        let canonical_bank = if self.opcode.addr_mode == AddrMode::AbsoluteLongIndexed
            && (0x80..=0xCF).contains(&(self.raw_operand >> 16))
            && (self.raw_operand & 0xFFFF) < 0x40 {
            // This is sometimes used for accessing struct fields relative to a pointer to
            // another bank, which would be incorrectly canonicalized as a WRAM mirror.
            label_bank.map(|(bank, _addr)| bank)
        } else {
            label_bank.map(|(bank, addr)| canonicalize_bank(addr_with_bank(bank, Addr::from(addr))))
        };

        canonical_bank
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
