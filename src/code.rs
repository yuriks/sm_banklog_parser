use std::fmt::Write;

use crate::label::LabelMap;
use crate::opcode::StaticAddress;
use crate::{
    config::Config,
    opcode::{AddrMode, Opcode},
    Addr, Bank, InstructionPrototype,
};

#[derive(Debug, Clone)]
pub enum ArgType {
    None,
    Address(u64),
    BlockMove(u8, u8),
}

#[derive(Debug, Clone)]
pub struct Code {
    pub address: u64,
    pub opcode: &'static Opcode,
    pub arg: ArgType,
    pub comment: Option<String>,
    pub length: u8,
    // TODO: Rename to "label_bank"
    pub db: Bank,

    pub instruction_prototype: Option<InstructionPrototype>,
}

impl Code {
    fn arg_label(&self, config: &Config, labels: &LabelMap) -> Option<String> {
        /* TODO: Get label if exists */
        /* Make sure to handle PC-relative addresses correctly */
        match self.arg {
            ArgType::Address(addr) => {
                let mut override_db = None;

                let override_ = config.get_override(self.address);
                if let Some(override_) = override_ {
                    override_db = override_.db;
                }

                let label_addr = self.static_label_address();
                if override_db.is_some()
                    && !matches!(
                        label_addr,
                        StaticAddress::Data(_) | StaticAddress::Immediate(_)
                    )
                {
                    eprintln!("Instruction at ${:06X} has operand DB override, but addressing mode doesn't depend on DB.", self.address);
                }
                let label_addr = match label_addr {
                    StaticAddress::None => None,
                    StaticAddress::Long(addr) => Some(addr),
                    StaticAddress::Data(addr) => {
                        Some((Addr::from(override_db.unwrap_or(self.db)) << 16) + Addr::from(addr))
                    }
                    StaticAddress::Immediate(imm) => override_db
                        .map(|label_bank| (Addr::from(label_bank) << 16) + Addr::from(imm)),
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
                            labels.get_label(label_addr.wrapping_add_signed(1)).map(|l| (l, 1))
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
                    _ => match self.length {
                        1 => format!("${addr:02X}"),
                        2 => format!("${addr:04X}"),
                        3 => format!("${addr:06X}"),
                        _ => panic!("Invalid argument length"),
                    },
                };

                Some(result)
            }
            ArgType::BlockMove(src, dst) => Some(format!("${src:02X},${dst:02X}")),
            ArgType::None => None,
        }
    }

    pub fn static_label_address(&self) -> StaticAddress {
        match self.arg {
            ArgType::Address(operand) => self.opcode.addr_mode.static_label_address(
                self.address,
                1 + u16::from(self.length),
                operand,
            ),
            ArgType::None | ArgType::BlockMove(_, _) => StaticAddress::None,
        }
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
            AddrMode::Immediate =>                      format!("{}.{} #{}", self.opcode.name, if self.length == 1 { "b" } else { "w" }, arg_label),
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
