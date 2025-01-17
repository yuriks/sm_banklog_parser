use seq_macro::seq;

use crate::{Addr, Bank};

const OPCODES: [Opcode; 0x100] = seq!(N in 0x0..=0xFF {[
    #(get_opcode(N),)*
]});

#[allow(clippy::too_many_lines)]
const fn get_opcode(op: u8) -> Opcode {
    match op {
        0x00 => Opcode::new(0x00, "BRK", AddrMode::ImmediateByte),

        0x61 => Opcode::new(0x61, "ADC", AddrMode::DirectIndexedIndirect),
        0x63 => Opcode::new(0x63, "ADC", AddrMode::StackRelative),
        0x65 => Opcode::new(0x65, "ADC", AddrMode::Direct),
        0x67 => Opcode::new(0x67, "ADC", AddrMode::DirectIndirectLong),
        0x69 => Opcode::new(0x69, "ADC", AddrMode::Immediate),
        0x6D => Opcode::new(0x6D, "ADC", AddrMode::Absolute),
        0x6F => Opcode::new(0x6F, "ADC", AddrMode::AbsoluteLong),
        0x71 => Opcode::new(0x71, "ADC", AddrMode::DirectIndirectIndexed),
        0x72 => Opcode::new(0x72, "ADC", AddrMode::DirectIndirect),
        0x73 => Opcode::new(0x73, "ADC", AddrMode::StackRelativeIndirectIndexed),
        0x75 => Opcode::new(0x75, "ADC", AddrMode::DirectIndexedX),
        0x77 => Opcode::new(0x77, "ADC", AddrMode::DirectIndirectLongIndexed),
        0x79 => Opcode::new(0x79, "ADC", AddrMode::AbsoluteIndexedY),
        0x7D => Opcode::new(0x7D, "ADC", AddrMode::AbsoluteIndexedX),
        0x7F => Opcode::new(0x7F, "ADC", AddrMode::AbsoluteLongIndexed),

        0x21 => Opcode::new(0x21, "AND", AddrMode::DirectIndexedIndirect),
        0x23 => Opcode::new(0x23, "AND", AddrMode::StackRelative),
        0x25 => Opcode::new(0x25, "AND", AddrMode::Direct),
        0x27 => Opcode::new(0x27, "AND", AddrMode::DirectIndirectLong),
        0x29 => Opcode::new(0x29, "AND", AddrMode::Immediate),
        0x2D => Opcode::new(0x2D, "AND", AddrMode::Absolute),
        0x2F => Opcode::new(0x2F, "AND", AddrMode::AbsoluteLong),
        0x31 => Opcode::new(0x31, "AND", AddrMode::DirectIndirectIndexed),
        0x32 => Opcode::new(0x32, "AND", AddrMode::DirectIndirect),
        0x33 => Opcode::new(0x33, "AND", AddrMode::StackRelativeIndirectIndexed),
        0x35 => Opcode::new(0x35, "AND", AddrMode::DirectIndexedX),
        0x37 => Opcode::new(0x37, "AND", AddrMode::DirectIndirectLongIndexed),
        0x39 => Opcode::new(0x39, "AND", AddrMode::AbsoluteIndexedY),
        0x3D => Opcode::new(0x3D, "AND", AddrMode::AbsoluteIndexedX),
        0x3F => Opcode::new(0x3F, "AND", AddrMode::AbsoluteLongIndexed),

        0xC1 => Opcode::new(0xC1, "CMP", AddrMode::DirectIndexedIndirect),
        0xC3 => Opcode::new(0xC3, "CMP", AddrMode::StackRelative),
        0xC5 => Opcode::new(0xC5, "CMP", AddrMode::Direct),
        0xC7 => Opcode::new(0xC7, "CMP", AddrMode::DirectIndirectLong),
        0xC9 => Opcode::new(0xC9, "CMP", AddrMode::Immediate),
        0xCD => Opcode::new(0xCD, "CMP", AddrMode::Absolute),
        0xCF => Opcode::new(0xCF, "CMP", AddrMode::AbsoluteLong),
        0xD1 => Opcode::new(0xD1, "CMP", AddrMode::DirectIndirectIndexed),
        0xD2 => Opcode::new(0xD2, "CMP", AddrMode::DirectIndirect),
        0xD3 => Opcode::new(0xD3, "CMP", AddrMode::StackRelativeIndirectIndexed),
        0xD5 => Opcode::new(0xD5, "CMP", AddrMode::DirectIndexedX),
        0xD7 => Opcode::new(0xD7, "CMP", AddrMode::DirectIndirectLongIndexed),
        0xD9 => Opcode::new(0xD9, "CMP", AddrMode::AbsoluteIndexedY),
        0xDD => Opcode::new(0xDD, "CMP", AddrMode::AbsoluteIndexedX),
        0xDF => Opcode::new(0xDF, "CMP", AddrMode::AbsoluteLongIndexed),

        0x41 => Opcode::new(0x41, "EOR", AddrMode::DirectIndexedIndirect),
        0x43 => Opcode::new(0x43, "EOR", AddrMode::StackRelative),
        0x45 => Opcode::new(0x45, "EOR", AddrMode::Direct),
        0x47 => Opcode::new(0x47, "EOR", AddrMode::DirectIndirectLong),
        0x49 => Opcode::new(0x49, "EOR", AddrMode::Immediate),
        0x4D => Opcode::new(0x4D, "EOR", AddrMode::Absolute),
        0x4F => Opcode::new(0x4F, "EOR", AddrMode::AbsoluteLong),
        0x51 => Opcode::new(0x51, "EOR", AddrMode::DirectIndirectIndexed),
        0x52 => Opcode::new(0x52, "EOR", AddrMode::DirectIndirect),
        0x53 => Opcode::new(0x53, "EOR", AddrMode::StackRelativeIndirectIndexed),
        0x55 => Opcode::new(0x55, "EOR", AddrMode::DirectIndexedX),
        0x57 => Opcode::new(0x57, "EOR", AddrMode::DirectIndirectLongIndexed),
        0x59 => Opcode::new(0x59, "EOR", AddrMode::AbsoluteIndexedY),
        0x5D => Opcode::new(0x5D, "EOR", AddrMode::AbsoluteIndexedX),
        0x5F => Opcode::new(0x5F, "EOR", AddrMode::AbsoluteLongIndexed),

        0xA1 => Opcode::new(0xA1, "LDA", AddrMode::DirectIndexedIndirect),
        0xA3 => Opcode::new(0xA3, "LDA", AddrMode::StackRelative),
        0xA5 => Opcode::new(0xA5, "LDA", AddrMode::Direct),
        0xA7 => Opcode::new(0xA7, "LDA", AddrMode::DirectIndirectLong),
        0xA9 => Opcode::new(0xA9, "LDA", AddrMode::Immediate),
        0xAD => Opcode::new(0xAD, "LDA", AddrMode::Absolute),
        0xAF => Opcode::new(0xAF, "LDA", AddrMode::AbsoluteLong),
        0xB1 => Opcode::new(0xB1, "LDA", AddrMode::DirectIndirectIndexed),
        0xB2 => Opcode::new(0xB2, "LDA", AddrMode::DirectIndirect),
        0xB3 => Opcode::new(0xB3, "LDA", AddrMode::StackRelativeIndirectIndexed),
        0xB5 => Opcode::new(0xB5, "LDA", AddrMode::DirectIndexedX),
        0xB7 => Opcode::new(0xB7, "LDA", AddrMode::DirectIndirectLongIndexed),
        0xB9 => Opcode::new(0xB9, "LDA", AddrMode::AbsoluteIndexedY),
        0xBD => Opcode::new(0xBD, "LDA", AddrMode::AbsoluteIndexedX),
        0xBF => Opcode::new(0xBF, "LDA", AddrMode::AbsoluteLongIndexed),

        0x01 => Opcode::new(0x01, "ORA", AddrMode::DirectIndexedIndirect),
        0x03 => Opcode::new(0x03, "ORA", AddrMode::StackRelative),
        0x05 => Opcode::new(0x05, "ORA", AddrMode::Direct),
        0x07 => Opcode::new(0x07, "ORA", AddrMode::DirectIndirectLong),
        0x09 => Opcode::new(0x09, "ORA", AddrMode::Immediate),
        0x0D => Opcode::new(0x0D, "ORA", AddrMode::Absolute),
        0x0F => Opcode::new(0x0F, "ORA", AddrMode::AbsoluteLong),
        0x11 => Opcode::new(0x11, "ORA", AddrMode::DirectIndirectIndexed),
        0x12 => Opcode::new(0x12, "ORA", AddrMode::DirectIndirect),
        0x13 => Opcode::new(0x13, "ORA", AddrMode::StackRelativeIndirectIndexed),
        0x15 => Opcode::new(0x15, "ORA", AddrMode::DirectIndexedX),
        0x17 => Opcode::new(0x17, "ORA", AddrMode::DirectIndirectLongIndexed),
        0x19 => Opcode::new(0x19, "ORA", AddrMode::AbsoluteIndexedY),
        0x1D => Opcode::new(0x1D, "ORA", AddrMode::AbsoluteIndexedX),
        0x1F => Opcode::new(0x1F, "ORA", AddrMode::AbsoluteLongIndexed),

        0xE1 => Opcode::new(0xE1, "SBC", AddrMode::DirectIndexedIndirect),
        0xE3 => Opcode::new(0xE3, "SBC", AddrMode::StackRelative),
        0xE5 => Opcode::new(0xE5, "SBC", AddrMode::Direct),
        0xE7 => Opcode::new(0xE7, "SBC", AddrMode::DirectIndirectLong),
        0xE9 => Opcode::new(0xE9, "SBC", AddrMode::Immediate),
        0xED => Opcode::new(0xED, "SBC", AddrMode::Absolute),
        0xEF => Opcode::new(0xEF, "SBC", AddrMode::AbsoluteLong),
        0xF1 => Opcode::new(0xF1, "SBC", AddrMode::DirectIndirectIndexed),
        0xF2 => Opcode::new(0xF2, "SBC", AddrMode::DirectIndirect),
        0xF3 => Opcode::new(0xF3, "SBC", AddrMode::StackRelativeIndirectIndexed),
        0xF5 => Opcode::new(0xF5, "SBC", AddrMode::DirectIndexedX),
        0xF7 => Opcode::new(0xF7, "SBC", AddrMode::DirectIndirectLongIndexed),
        0xF9 => Opcode::new(0xF9, "SBC", AddrMode::AbsoluteIndexedY),
        0xFD => Opcode::new(0xFD, "SBC", AddrMode::AbsoluteIndexedX),
        0xFF => Opcode::new(0xFF, "SBC", AddrMode::AbsoluteLongIndexed),

        0x81 => Opcode::new(0x81, "STA", AddrMode::DirectIndexedIndirect),
        0x83 => Opcode::new(0x83, "STA", AddrMode::StackRelative),
        0x85 => Opcode::new(0x85, "STA", AddrMode::Direct),
        0x87 => Opcode::new(0x87, "STA", AddrMode::DirectIndirectLong),
        0x8D => Opcode::new(0x8D, "STA", AddrMode::Absolute),
        0x8F => Opcode::new(0x8F, "STA", AddrMode::AbsoluteLong),
        0x91 => Opcode::new(0x91, "STA", AddrMode::DirectIndirectIndexed),
        0x92 => Opcode::new(0x92, "STA", AddrMode::DirectIndirect),
        0x93 => Opcode::new(0x93, "STA", AddrMode::StackRelativeIndirectIndexed),
        0x95 => Opcode::new(0x95, "STA", AddrMode::DirectIndexedX),
        0x97 => Opcode::new(0x97, "STA", AddrMode::DirectIndirectLongIndexed),
        0x99 => Opcode::new(0x99, "STA", AddrMode::AbsoluteIndexedY),
        0x9D => Opcode::new(0x9D, "STA", AddrMode::AbsoluteIndexedX),
        0x9F => Opcode::new(0x9F, "STA", AddrMode::AbsoluteLongIndexed),

        0x06 => Opcode::new(0x06, "ASL", AddrMode::Direct),
        0x0A => Opcode::new(0x0A, "ASL", AddrMode::Implied),
        0x0E => Opcode::new(0x0E, "ASL", AddrMode::Absolute),
        0x16 => Opcode::new(0x16, "ASL", AddrMode::DirectIndexedX),
        0x1E => Opcode::new(0x1E, "ASL", AddrMode::AbsoluteIndexedX),

        0x46 => Opcode::new(0x46, "LSR", AddrMode::Direct),
        0x4A => Opcode::new(0x4A, "LSR", AddrMode::Implied),
        0x4E => Opcode::new(0x4E, "LSR", AddrMode::Absolute),
        0x56 => Opcode::new(0x56, "LSR", AddrMode::DirectIndexedX),
        0x5E => Opcode::new(0x5E, "LSR", AddrMode::AbsoluteIndexedX),

        0x26 => Opcode::new(0x26, "ROL", AddrMode::Direct),
        0x2A => Opcode::new(0x2A, "ROL", AddrMode::Implied),
        0x2E => Opcode::new(0x2E, "ROL", AddrMode::Absolute),
        0x36 => Opcode::new(0x36, "ROL", AddrMode::DirectIndexedX),
        0x3E => Opcode::new(0x3E, "ROL", AddrMode::AbsoluteIndexedX),

        0x66 => Opcode::new(0x66, "ROR", AddrMode::Direct),
        0x6A => Opcode::new(0x6A, "ROR", AddrMode::Implied),
        0x6E => Opcode::new(0x6E, "ROR", AddrMode::Absolute),
        0x76 => Opcode::new(0x76, "ROR", AddrMode::DirectIndexedX),
        0x7E => Opcode::new(0x7E, "ROR", AddrMode::AbsoluteIndexedX),

        0x24 => Opcode::new(0x24, "BIT", AddrMode::Direct),
        0x2C => Opcode::new(0x2C, "BIT", AddrMode::Absolute),
        0x34 => Opcode::new(0x34, "BIT", AddrMode::DirectIndexedX),
        0x3C => Opcode::new(0x3C, "BIT", AddrMode::AbsoluteIndexedX),
        0x89 => Opcode::new(0x89, "BIT", AddrMode::Immediate),

        0xE0 => Opcode::new(0xE0, "CPX", AddrMode::Immediate),
        0xE4 => Opcode::new(0xE4, "CPX", AddrMode::Direct),
        0xEC => Opcode::new(0xEC, "CPX", AddrMode::Absolute),

        0xC0 => Opcode::new(0xC0, "CPY", AddrMode::Immediate),
        0xC4 => Opcode::new(0xC4, "CPY", AddrMode::Direct),
        0xCC => Opcode::new(0xCC, "CPY", AddrMode::Absolute),

        0x3A => Opcode::new(0x3A, "DEC", AddrMode::Implied),
        0xC6 => Opcode::new(0xC6, "DEC", AddrMode::Direct),
        0xCE => Opcode::new(0xCE, "DEC", AddrMode::Absolute),
        0xD6 => Opcode::new(0xD6, "DEC", AddrMode::DirectIndexedX),
        0xDE => Opcode::new(0xDE, "DEC", AddrMode::AbsoluteIndexedX),

        0x1A => Opcode::new(0x1A, "INC", AddrMode::Implied),
        0xE6 => Opcode::new(0xE6, "INC", AddrMode::Direct),
        0xEE => Opcode::new(0xEE, "INC", AddrMode::Absolute),
        0xF6 => Opcode::new(0xF6, "INC", AddrMode::DirectIndexedX),
        0xFE => Opcode::new(0xFE, "INC", AddrMode::AbsoluteIndexedX),

        0xA2 => Opcode::new(0xA2, "LDX", AddrMode::Immediate),
        0xA6 => Opcode::new(0xA6, "LDX", AddrMode::Direct),
        0xAE => Opcode::new(0xAE, "LDX", AddrMode::Absolute),
        0xB6 => Opcode::new(0xB6, "LDX", AddrMode::DirectIndexedY),
        0xBE => Opcode::new(0xBE, "LDX", AddrMode::AbsoluteIndexedY),

        0xA0 => Opcode::new(0xA0, "LDY", AddrMode::Immediate),
        0xA4 => Opcode::new(0xA4, "LDY", AddrMode::Direct),
        0xAC => Opcode::new(0xAC, "LDY", AddrMode::Absolute),
        0xB4 => Opcode::new(0xB4, "LDY", AddrMode::DirectIndexedX),
        0xBC => Opcode::new(0xBC, "LDY", AddrMode::AbsoluteIndexedX),

        0x86 => Opcode::new(0x86, "STX", AddrMode::Direct),
        0x8E => Opcode::new(0x8E, "STX", AddrMode::Absolute),
        0x96 => Opcode::new(0x96, "STX", AddrMode::DirectIndexedY),

        0x84 => Opcode::new(0x84, "STY", AddrMode::Direct),
        0x8C => Opcode::new(0x8C, "STY", AddrMode::Absolute),
        0x94 => Opcode::new(0x94, "STY", AddrMode::DirectIndexedX),

        0x64 => Opcode::new(0x64, "STZ", AddrMode::Direct),
        0x74 => Opcode::new(0x74, "STZ", AddrMode::DirectIndexedX),
        0x9C => Opcode::new(0x9C, "STZ", AddrMode::Absolute),
        0x9E => Opcode::new(0x9E, "STZ", AddrMode::AbsoluteIndexedX),

        0x90 => Opcode::new(0x90, "BCC", AddrMode::PcRelative),
        0xB0 => Opcode::new(0xB0, "BCS", AddrMode::PcRelative),
        0xF0 => Opcode::new(0xF0, "BEQ", AddrMode::PcRelative),
        0x30 => Opcode::new(0x30, "BMI", AddrMode::PcRelative),
        0xD0 => Opcode::new(0xD0, "BNE", AddrMode::PcRelative),
        0x10 => Opcode::new(0x10, "BPL", AddrMode::PcRelative),
        0x80 => Opcode::new(0x80, "BRA", AddrMode::PcRelative),
        0x50 => Opcode::new(0x50, "BVC", AddrMode::PcRelative),
        0x70 => Opcode::new(0x70, "BVS", AddrMode::PcRelative),

        0x82 => Opcode::new(0x82, "BRL", AddrMode::PcRelativeLong),

        0x18 => Opcode::new(0x18, "CLC", AddrMode::Implied),
        0xD8 => Opcode::new(0xD8, "CLD", AddrMode::Implied),
        0x58 => Opcode::new(0x58, "CLI", AddrMode::Implied),
        0xB8 => Opcode::new(0xB8, "CLV", AddrMode::Implied),

        0x02 => Opcode::new(0x02, "COP", AddrMode::ImmediateByte),

        0xCA => Opcode::new(0xCA, "DEX", AddrMode::Implied),
        0x88 => Opcode::new(0x88, "DEY", AddrMode::Implied),

        0xE8 => Opcode::new(0xE8, "INX", AddrMode::Implied),
        0xC8 => Opcode::new(0xC8, "INY", AddrMode::Implied),

        0x4C => Opcode::new(0x4C, "JMP", AddrMode::CodeAbsolute),
        0x5C => Opcode::new(0x5C, "JML", AddrMode::AbsoluteLong),
        0x6C => Opcode::new(0x6C, "JMP", AddrMode::AbsoluteIndirect),
        0x7C => Opcode::new(0x7C, "JMP", AddrMode::AbsoluteIndexedIndirect),
        0xDC => Opcode::new(0xDC, "JML", AddrMode::AbsoluteIndirectLong),

        0x20 => Opcode::new(0x20, "JSR", AddrMode::CodeAbsolute),
        0x22 => Opcode::new(0x22, "JSL", AddrMode::AbsoluteLong),
        0xFC => Opcode::new(0xFC, "JSR", AddrMode::AbsoluteIndexedIndirect),

        0x54 => Opcode::new(0x54, "MVN", AddrMode::BlockMove),
        0x44 => Opcode::new(0x44, "MVP", AddrMode::BlockMove),

        0xEA => Opcode::new(0xEA, "NOP", AddrMode::Implied),

        0xF4 => Opcode::new(0xF4, "PEA", AddrMode::Absolute),
        0xD4 => Opcode::new(0xD4, "PEI", AddrMode::DirectIndirect),
        0x62 => Opcode::new(0x62, "PER", AddrMode::PcRelativeLong),

        0x48 => Opcode::new(0x48, "PHA", AddrMode::Implied),
        0x8B => Opcode::new(0x8B, "PHB", AddrMode::Implied),
        0x0B => Opcode::new(0x0B, "PHD", AddrMode::Implied),
        0x4B => Opcode::new(0x4B, "PHK", AddrMode::Implied),
        0x08 => Opcode::new(0x08, "PHP", AddrMode::Implied),
        0xDA => Opcode::new(0xDA, "PHX", AddrMode::Implied),
        0x5A => Opcode::new(0x5A, "PHY", AddrMode::Implied),

        0x68 => Opcode::new(0x68, "PLA", AddrMode::Implied),
        0xAB => Opcode::new(0xAB, "PLB", AddrMode::Implied),
        0x2B => Opcode::new(0x2B, "PLD", AddrMode::Implied),
        0x28 => Opcode::new(0x28, "PLP", AddrMode::Implied),
        0xFA => Opcode::new(0xFA, "PLX", AddrMode::Implied),
        0x7A => Opcode::new(0x7A, "PLY", AddrMode::Implied),

        0xC2 => Opcode::new(0xC2, "REP", AddrMode::ImmediateByte),
        0xE2 => Opcode::new(0xE2, "SEP", AddrMode::ImmediateByte),

        0x40 => Opcode::new(0x40, "RTI", AddrMode::Implied),
        0x6B => Opcode::new(0x6B, "RTL", AddrMode::Implied),
        0x60 => Opcode::new(0x60, "RTS", AddrMode::Implied),

        0x38 => Opcode::new(0x38, "SEC", AddrMode::Implied),
        0xF8 => Opcode::new(0xF8, "SED", AddrMode::Implied),
        0x78 => Opcode::new(0x78, "SEI", AddrMode::Implied),

        0xDB => Opcode::new(0xDB, "STP", AddrMode::Implied),

        0xAA => Opcode::new(0xAA, "TAX", AddrMode::Implied),
        0xA8 => Opcode::new(0xA8, "TAY", AddrMode::Implied),
        0x5B => Opcode::new(0x5B, "TCD", AddrMode::Implied),
        0x1B => Opcode::new(0x1B, "TCS", AddrMode::Implied),
        0x7B => Opcode::new(0x7B, "TDC", AddrMode::Implied),
        0x3B => Opcode::new(0x3B, "TSC", AddrMode::Implied),
        0xBA => Opcode::new(0xBA, "TSX", AddrMode::Implied),
        0x8A => Opcode::new(0x8A, "TXA", AddrMode::Implied),
        0x9A => Opcode::new(0x9A, "TXS", AddrMode::Implied),
        0x9B => Opcode::new(0x9B, "TXY", AddrMode::Implied),
        0x98 => Opcode::new(0x98, "TYA", AddrMode::Implied),
        0xBB => Opcode::new(0xBB, "TYX", AddrMode::Implied),

        0x14 => Opcode::new(0x14, "TRB", AddrMode::Direct),
        0x1C => Opcode::new(0x1C, "TRB", AddrMode::Absolute),

        0x04 => Opcode::new(0x04, "TSB", AddrMode::Direct),
        0x0C => Opcode::new(0x0C, "TSB", AddrMode::Absolute),

        0xCB => Opcode::new(0xCB, "WAI", AddrMode::Implied),
        0x42 => Opcode::new(0x42, "WDM", AddrMode::ImmediateByte),

        0xEB => Opcode::new(0xEB, "XBA", AddrMode::Implied),
        0xFB => Opcode::new(0xFB, "XCE", AddrMode::Implied),

        // RustRover reports a false positive error without this:
        // "The `match` expression does not cover all possible cases [E0004]"
        #[allow(unreachable_patterns)]
        _ => unreachable!(),
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum AddrMode {
    Implied,
    /// Immediate operand whose size depends on the current flags
    Immediate,
    /// Immediate operand which is always byte-sized independently of flags
    ImmediateByte,
    PcRelative,
    PcRelativeLong,
    Direct,
    DirectIndexedX,
    DirectIndexedY,
    DirectIndirect,
    DirectIndexedIndirect,
    DirectIndirectIndexed,
    DirectIndirectLong,
    DirectIndirectLongIndexed,
    Absolute,
    /// Same as Absolute, but uses the Program Bank register for destination
    CodeAbsolute,
    AbsoluteIndexedX,
    AbsoluteIndexedY,
    AbsoluteLong,
    AbsoluteLongIndexed,
    StackRelative,
    StackRelativeIndirectIndexed,
    AbsoluteIndirect,
    AbsoluteIndirectLong,
    AbsoluteIndexedIndirect,
    BlockMove,
}

#[derive(Copy, Clone, Debug)]
pub enum StaticAddress {
    /// No static addressing info
    None,
    /// Full 24-bit address
    Long(Addr),
    /// 16-bit address in the current Data Bank
    DataBank(u16),
    /// Immediate value or stack offset (may be mapped into a label by an override)
    Immediate(u16),
    /// Block move, the odd one out
    BlockMove { src: u8, dst: u8 },
}

impl AddrMode {
    pub fn static_label_address(
        self,
        instr_addr: Addr,
        instr_length: u16,
        operand: u32,
    ) -> StaticAddress {
        fn addr_in_bank(bank: Bank, addr: u16) -> StaticAddress {
            StaticAddress::Long((Addr::from(bank) << 16) + Addr::from(addr))
        }

        let program_bank = (instr_addr >> 16) as Bank;
        let pc = instr_addr as u16;
        let next_pc = pc.wrapping_add(instr_length);
        let dp_base = 0u16;

        let operand_b = (operand & 0xFF) as u8;
        let operand_sb = operand_b as i8;
        let operand_w = (operand & 0xFFFF) as u16;
        let operand_sw = operand_w as i16;
        let operand_l = operand & 0xFF_FFFF;

        match self {
            AddrMode::Immediate
            | AddrMode::ImmediateByte
            | AddrMode::StackRelative
            | AddrMode::StackRelativeIndirectIndexed => StaticAddress::Immediate(operand_w),

            AddrMode::Implied => StaticAddress::None,

            AddrMode::BlockMove => StaticAddress::BlockMove {
                src: (operand_w & 0x00FF) as u8,
                dst: (operand_w >> 8) as u8,
            },

            AddrMode::PcRelative => addr_in_bank(
                program_bank,
                next_pc.wrapping_add_signed(i16::from(operand_sb)),
            ),
            AddrMode::PcRelativeLong => {
                addr_in_bank(program_bank, next_pc.wrapping_add_signed(operand_sw))
            }

            AddrMode::Direct
            | AddrMode::DirectIndexedX
            | AddrMode::DirectIndexedY
            | AddrMode::DirectIndirect
            | AddrMode::DirectIndexedIndirect
            | AddrMode::DirectIndirectIndexed
            | AddrMode::DirectIndirectLong
            | AddrMode::DirectIndirectLongIndexed => {
                addr_in_bank(0x00, dp_base.wrapping_add(u16::from(operand_b)))
            }

            AddrMode::Absolute | AddrMode::AbsoluteIndexedX | AddrMode::AbsoluteIndexedY => {
                StaticAddress::DataBank(operand_w)
            }
            AddrMode::AbsoluteLong | AddrMode::AbsoluteLongIndexed => {
                StaticAddress::Long(Addr::from(operand_l))
            }
            AddrMode::CodeAbsolute | AddrMode::AbsoluteIndexedIndirect => {
                addr_in_bank(program_bank, operand_w)
            }
            AddrMode::AbsoluteIndirect | AddrMode::AbsoluteIndirectLong => {
                addr_in_bank(0x00, operand_w)
            }
        }
    }
}

#[derive(Debug)]
pub struct Opcode {
    pub opcode: u8,
    pub name: &'static str,
    pub addr_mode: AddrMode,
}

impl Opcode {
    const fn new(opcode: u8, name: &'static str, addr_mode: AddrMode) -> Self {
        Self {
            opcode,
            name,
            addr_mode,
        }
    }

    pub fn get(op_byte: u8) -> &'static Opcode {
        &OPCODES[op_byte as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::Opcode;

    #[test]
    fn test_opcode_get() {
        // Check that every opcode is at its correct index
        for i in 0..=u8::MAX {
            let op = Opcode::get(i);
            assert_eq!(
                i, op.opcode,
                "Opcode table op byte mismatch: [{i:#X}] = {op:X?}"
            );
        }
    }
}
