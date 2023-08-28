use std::fmt::Write;

use crate::{config::Config, label::LabelType, opcode::{AddrMode, Opcode}};
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
}

impl Code {
    fn arg_label(&self, config: &Config, labels: &mut LabelMap) -> Option<String> {
        /* TODO: Get label if exists */
        /* Make sure to handle PC-relative addresses correctly */
        match self.arg {
            ArgType::Address(addr) => {
                let label_addr = match self.opcode.addr_mode {
                    AddrMode::Relative => {
                        ((self.address as i64) + 2 + ((addr & 0xFF) as i8) as i64) as u64
                    },
                    AddrMode::RelativeLong => {
                        ((self.address as i64) + 2 + ((addr & 0xFFFF) as i16) as i64) as u64
                    },
                    _ => {
                        match self.length {
                            1 => 0x7E0000 | (addr & 0xFF),
                            2 => match addr {
                                0..=0x1FFF => 0x7E0000 | (addr & 0xFFFF),
                                0x2000..=0x7FFF => addr & 0xFFFF,
                                _ => ((self.db as u64) << 16) | (addr & 0xFFFF)
                            },
                            3 => addr,
                            _ => panic!("Invalid argument length")
                        }
                    }
                };

                let label = {
                    if labels.contains_key(&label_addr) {
                        (Some(labels.get_mut(&label_addr).unwrap()), 0)
                    } else if self.opcode.addr_mode != AddrMode::Relative &&
                                self.opcode.addr_mode != AddrMode::RelativeLong &&
                                self.opcode.name != "JSR" &&
                                self.opcode.name != "JSL"
                        {
                        if labels.contains_key(&(label_addr - 1)) {
                            (Some(labels.get_mut(&(label_addr - 1)).unwrap()), -1)
                        } else if labels.contains_key(&(label_addr + 1)) {
                            (Some(labels.get_mut(&(label_addr + 1)).unwrap()), 1)
                        } else if labels.contains_key(&(label_addr - 2)) {
                            (Some(labels.get_mut(&(label_addr - 2)).unwrap()), -2)
                        } else if labels.contains_key(&(label_addr + 2)) {
                            (Some(labels.get_mut(&(label_addr + 2)).unwrap()), 2)
                        } else {
                            (None, 0)                         
                        }
                    } else {
                        (None, 0)    
                    }
                };

                let result = match label {
                    (Some(l), offset) => {
                        l.use_from(self.address);

                        if (((self.opcode.addr_mode == AddrMode::Immediate || self.opcode.addr_mode == AddrMode::ImmediateByte) && config.get_override(self.address).is_some()) || (self.opcode.addr_mode != AddrMode::Immediate && self.opcode.addr_mode != AddrMode::ImmediateByte)) && l.label_type != LabelType::Blocked {
                            match offset {
                                0 => l.name.to_string(),
                                -1 | -2 => format!("{}+{}", l.name, -offset),
                                1 | 2 => format!("{}{}", l.name, -offset),
                                _ => panic!("Invalid argument length")
                            }
                        } else {
                            match self.length {
                                1 => format!("${:02X}", addr),
                                2 => format!("${:04X}", addr),
                                3 => format!("${:06X}", addr),
                                _ => panic!("Invalid argument length")
                            }                                
                        }
                    },
                    (None, _) => {
                        match self.length {
                            1 => format!("${:02X}", addr),
                            2 => format!("${:04X}", addr),
                            3 => format!("${:06X}", addr),
                            _ => panic!("Invalid argument length")
                        }
                    }
                };

                Some(result)
            },
            ArgType::BlockMove(src, dst) => {
                Some(format!("${:02X},${:02X}", src, dst))
            },
            _ => None
        }
    }
}

impl Code {
    pub fn to_string(&self, config: &Config, labels: &mut LabelMap) -> String {
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
            write!(&mut result, " {}", comment).unwrap();
        }

        result
    }
}