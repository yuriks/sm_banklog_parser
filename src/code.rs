use crate::label::LabelMap;
use crate::opcode::StaticAddress;
use crate::operand::{format_address_expression_str, LabelOrLiteral, Override, OverrideMap};
use crate::{
    addr16_with_bank,
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

    pub logged_bank: Option<Bank>,

    pub instruction_prototype: Option<InstructionPrototype>,
}

impl Code {
    pub fn pc_advance(&self) -> u64 {
        1 + u64::from(self.operand_size)
    }

    fn arg_label(&self, overrides: &OverrideMap, labels: &LabelMap) -> Option<String> {
        let operand_value = self.get_operand();
        let target = match operand_value {
            StaticAddress::None => return None,
            StaticAddress::BlockMove { src, dst } => {
                return Some(format!("${src:02X},${dst:02X}"));
            }
            StaticAddress::Long(addr) => addr,

            StaticAddress::DataBank(low_addr) | StaticAddress::Immediate(low_addr) => {
                Addr::from(low_addr)
            }
        };

        let override_ = overrides.get_override(self.address);
        let label_addr = self.get_operand_label_address(override_);

        let disallow_fuzzy = self.opcode.addr_mode == AddrMode::PcRelative
            || self.opcode.addr_mode == AddrMode::PcRelativeLong
            || self.opcode.name == "JSR"
            || self.opcode.name == "JSL";

        let label = label_addr.and_then(|label_addr| {
            let l = if disallow_fuzzy {
                labels.get_label(label_addr)
            } else {
                labels.get_label_fuzzy(label_addr)
            };
            l?.attempt_use(self.address)
        });

        let base = label.map(LabelOrLiteral::Label);
        Some(format_address_expression_str(
            target,
            base,
            self.operand_size,
        ))
    }

    pub fn get_operand(&self) -> StaticAddress {
        self.opcode.addr_mode.static_label_address(
            self.address,
            1 + u16::from(self.operand_size),
            self.raw_operand,
        )
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
    pub fn to_string(&self, overrides: &OverrideMap, labels: &LabelMap) -> String {
        let op = self.opcode.name;
        let arg = self.arg_label(overrides, labels).unwrap_or_default();
        #[rustfmt::skip]
        let result = match self.opcode.addr_mode {
            AddrMode::Absolute
            | AddrMode::CodeAbsolute               => format!("    {op}.w {arg}"),
            AddrMode::AbsoluteIndexedIndirect      => format!("    {op}.w ({arg},X)"),
            AddrMode::AbsoluteLongIndexed          => format!("    {op}.l {arg},X"),
            AddrMode::AbsoluteIndexedX             => format!("    {op}.w {arg},X"),
            AddrMode::AbsoluteIndexedY             => format!("    {op}.w {arg},Y"),
            AddrMode::AbsoluteIndirect             => format!("    {op}.w ({arg})"),
            AddrMode::AbsoluteIndirectLong         => format!("    {op}.w [{arg}]"),
            AddrMode::AbsoluteLong                 => format!("    {op}.l {arg}"),
            AddrMode::BlockMove                    => format!("    {op} {arg}"),
            AddrMode::Direct                       => format!("    {op}.b {arg}"),
            AddrMode::DirectIndexedIndirect        => format!("    {op}.b ({arg},X)"),
            AddrMode::DirectIndexedX               => format!("    {op}.b {arg},X"),
            AddrMode::DirectIndexedY               => format!("    {op}.b {arg},Y"),
            AddrMode::DirectIndirect               => format!("    {op}.b ({arg})"),
            AddrMode::DirectIndirectIndexed        => format!("    {op}.b ({arg}),Y"),
            AddrMode::DirectIndirectLongIndexed    => format!("    {op}.b [{arg}],Y"),
            AddrMode::DirectIndirectLong           => format!("    {op}.b [{arg}]"),
            AddrMode::Immediate                    => format!("    {op}.{} #{arg}", if self.operand_size == 1 { "b" } else { "w" }),
            AddrMode::ImmediateByte                => format!("    {op}.b #{arg}"),
            AddrMode::Implied                      => format!("    {op}"),
            AddrMode::PcRelative                   => format!("    {op} {arg}"),
            AddrMode::PcRelativeLong               => format!("    {op} {arg}"),
            AddrMode::StackRelative                => format!("    {op}.b {arg},S"),
            AddrMode::StackRelativeIndirectIndexed => format!("    {op}.b ({arg},S),Y"),
        };
        result
    }
}
