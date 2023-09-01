use std::fmt::Write;

use crate::{config::Config, InstructionPrototype, opcode::{AddrMode, Opcode}};
use crate::label::LabelMap;

#[derive(Debug, Clone)]
pub enum ArgType {
    None,
    Address(u64),
    BlockMove(u8, u8)
}

#[derive(Debug, Clone)]
pub struct Code {
    pub address: u64,
    pub opcode: &'static Opcode,
    pub arg: ArgType,
    pub comment: Option<String>,
    pub length: u8,
    pub db: u8,

    pub instruction_prototype: Option<InstructionPrototype>,
}

impl Code {
    fn arg_label(&self, config: &Config, labels: &LabelMap) -> Option<String> {
        /* TODO: Get label if exists */
        /* Make sure to handle PC-relative addresses correctly */
        match self.arg {
            ArgType::Address(addr) => {
                let label_addr = match self.opcode.addr_mode {
                    AddrMode::Relative => {
                        ((self.address as i64) + 2 + i64::from((addr & 0xFF) as i8)) as u64
                    },
                    AddrMode::RelativeLong => {
                        ((self.address as i64) + 2 + i64::from((addr & 0xFFFF) as i16)) as u64
                    },
                    _ => {
                        match self.length {
                            1 => 0x7E_0000 | (addr & 0xFF),
                            2 => match addr {
                                0..=0x1FFF => 0x7E_0000 | (addr & 0xFFFF),
                                0x2000..=0x7FFF => addr & 0xFFFF,
                                _ => (u64::from(self.db) << 16) | (addr & 0xFFFF)
                            },
                            3 => addr,
                            _ => panic!("Invalid argument length")
                        }
                    }
                };

                let label = if self.opcode.addr_mode != AddrMode::Relative &&
                    self.opcode.addr_mode != AddrMode::RelativeLong &&
                    self.opcode.name != "JSR" &&
                    self.opcode.name != "JSL"
                {
                    labels.get_label_fuzzy(label_addr)
                } else {
                    labels.get_label(label_addr).map(|l| (l, 0))
                };

                let result = match label {
                    Some((l, offset)) => {
                        l.use_from(self.address);

                        if (((self.opcode.addr_mode == AddrMode::Immediate || self.opcode.addr_mode == AddrMode::ImmediateByte) && config.get_override(self.address).is_some()) || (self.opcode.addr_mode != AddrMode::Immediate && self.opcode.addr_mode != AddrMode::ImmediateByte)) && !l.is_blocked() {
                            match offset {
                                0 => l.name.to_string(),
                                -1 | -2 => format!("{}+{}", l.name, -offset),
                                1 | 2 => format!("{}{}", l.name, -offset),
                                _ => panic!("Invalid argument length")
                            }
                        } else {
                            match self.length {
                                1 => format!("${addr:02X}"),
                                2 => format!("${addr:04X}"),
                                3 => format!("${addr:06X}"),
                                _ => panic!("Invalid argument length")
                            }
                        }
                    },
                    None => {
                        match self.length {
                            1 => format!("${addr:02X}"),
                            2 => format!("${addr:04X}"),
                            3 => format!("${addr:06X}"),
                            _ => panic!("Invalid argument length")
                        }
                    }
                };

                Some(result)
            },
            ArgType::BlockMove(src, dst) => {
                Some(format!("${src:02X},${dst:02X}"))
            },
            ArgType::None => None
        }
    }
}

impl Code {
    //noinspection IncorrectFormatting
    #[allow(clippy::match_same_arms)]
    pub fn to_string(&self, config: &Config, labels: &LabelMap) -> String {
        let arg_label = self.arg_label(config, labels).unwrap_or_default();
        let opcode = match self.opcode.addr_mode {
            AddrMode::Absolute =>                       format!("{}.w {}", self.opcode.name, arg_label),
            AddrMode::AbsoluteIndexedIndirect =>        format!("{}.w ({},X)", self.opcode.name, arg_label),
            AddrMode::AbsoluteIndexedLong =>            format!("{}.l {},X", self.opcode.name, arg_label),
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
            AddrMode::DirectIndirectIndexedLong =>      format!("{}.b [{}],Y", self.opcode.name, arg_label),
            AddrMode::DirectIndirectLong =>             format!("{}.b [{}]", self.opcode.name, arg_label),
            AddrMode::Immediate =>                      format!("{}.{} #{}", self.opcode.name, if self.length == 1 { "b" } else { "w" }, arg_label),
            AddrMode::ImmediateByte =>                  format!("{}.b #{}", self.opcode.name, arg_label),
            AddrMode::Implied =>                        self.opcode.name.to_string(),
            AddrMode::Relative =>                       format!("{} {}", self.opcode.name, arg_label),
            AddrMode::RelativeLong =>                   format!("{} {}", self.opcode.name, arg_label),
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