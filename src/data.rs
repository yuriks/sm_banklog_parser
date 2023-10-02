use std::fmt::Write;

use crate::config::{Config, OperandType, Override};
use crate::directives::InstructionPrototype;
use crate::label::{format_address_expression, LabelMap, LabelOrLiteral, LabelType};
use crate::{addr16_with_bank, split_addr16, Addr, SpecialParsingType};

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub enum DataVal {
    DB(u8),
    DW(u16),
    DL(u32),
}

impl DataVal {
    pub fn get_db(self) -> Option<u8> {
        match self {
            DataVal::DB(x) => Some(x),
            _ => None,
        }
    }

    pub fn get_dw(self) -> Option<u16> {
        match self {
            DataVal::DW(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_u64(self) -> u64 {
        match self {
            DataVal::DB(b) => u64::from(b),
            DataVal::DW(w) => u64::from(w),
            DataVal::DL(l) => u64::from(l),
        }
    }

    pub fn length(self) -> u64 {
        match self {
            DataVal::DB(_) => 1,
            DataVal::DW(_) => 2,
            DataVal::DL(_) => 3,
        }
    }

    pub fn mnemonic(self) -> &'static str {
        match self {
            DataVal::DB(_) => "db",
            DataVal::DW(_) => "dw",
            DataVal::DL(_) => "dl",
        }
    }
}

#[derive(Debug, Clone)]
pub struct Data {
    pub address: Addr,
    pub data: Vec<DataVal>,
    pub comment: Option<String>,
    pub special_type: Option<SpecialParsingType>,
}

impl Data {
    pub fn pc_advance(&self) -> u64 {
        self.data.iter().map(|d| d.length()).sum()
    }

    pub fn generate_spritemap(&self) -> Result<Vec<String>, String> {
        let mut result = Vec::new();
        let mut it = self.data.iter();

        let count = it
            .next()
            .and_then(|d| d.get_dw())
            .ok_or("expected dw count")?;
        result.push(format!("dw ${count:04X}"));

        for _ in 0..count {
            let val1 = it
                .next()
                .and_then(|d| d.get_dw())
                .ok_or("expected dw value 1")?;
            let val2 = it
                .next()
                .and_then(|d| d.get_db())
                .ok_or("expected db value 2")?;
            let val3 = it
                .next()
                .and_then(|d| d.get_dw())
                .ok_or("expected dw value 3")?;

            // Use same-line operator to group with count if spritemap only has 1 entry
            if count == 1 {
                result[0].push_str(" : ");
            } else {
                result.push(String::new());
            }
            let l = result.last_mut().unwrap();

            let val_x = val1 & 0x7FFF;
            let val_large = (val1 >> 15) & 1;

            let val_y = val2;

            let val_tile = val3 & 0x1FF;
            let val_pal = (val3 >> 9) & 7;
            let val_prio = (val3 >> 12) & 3;
            let val_xflip = (val3 >> 14) & 1;
            let val_yflip = (val3 >> 15) & 1;

            write!(
                l,
                "%spritemap_entry({val_x:3}, {val_y:3}, ${val_tile:03X}, "
            )
            .unwrap();

            let mut wrote_flag = false;
            macro_rules! add_and {
                () => {
                    if wrote_flag {
                        l.push_str("|");
                    }
                    wrote_flag = true;
                };
            }

            if val_pal != 0 {
                wrote_flag = true;
                write!(l, "SPRM_PAL({val_pal})").unwrap();
            }

            if val_prio != 0 {
                add_and!();
                write!(l, "SPRM_PRIO({val_prio})").unwrap();
            }
            if val_large != 0 {
                add_and!();
                l.push_str("!SPRM_LARGE");
            }
            if val_xflip != 0 {
                add_and!();
                l.push_str("!SPRM_XFLIP");
            }
            if val_yflip != 0 {
                add_and!();
                l.push_str("!SPRM_YFLIP");
            }
            if !wrote_flag {
                l.push('0');
            }

            l.push(')');
        }

        if it.next().is_some() {
            return Err("trailing data".into());
        }

        Ok(result)
    }

    pub fn generate_raw_spritemap(&self) -> Result<Vec<String>, String> {
        let mut result = Vec::new();
        let mut it = self.data.iter();

        let count = it
            .next()
            .and_then(|d| d.get_dw())
            .ok_or("expected dw count")?;
        result.push(format!("dw ${count:04X}"));

        for i in 0..count {
            let val1 = it
                .next()
                .and_then(|d| d.get_dw())
                .ok_or("expected dw value 1")?;
            let val2 = it
                .next()
                .and_then(|d| d.get_db())
                .ok_or("expected db value 2")?;
            let val3 = it
                .next()
                .and_then(|d| d.get_dw())
                .ok_or("expected dw value 3")?;

            // Write 4 entries per line
            let new_line = (count > 2) && (i % 2 == 0);
            if new_line {
                result.push(String::new());
            }
            let l = result.last_mut().unwrap();
            if !new_line {
                l.push_str(" : ");
            }

            write!(l, "%spritemap_raw(${val1:04X}, ${val2:02X}, ${val3:04X})").unwrap();
        }

        if it.next().is_some() {
            return Err("trailing data".into());
        }

        Ok(result)
    }

    fn generate_instruction_list(&self, labels: &LabelMap) -> Result<Vec<String>, String> {
        let mut result = Vec::new();

        let mut it = self.data.iter();
        while let Some(instruction) = it.next() {
            let default_prototype = InstructionPrototype { params: Vec::new() };

            let instruction = instruction.get_dw().ok_or("Instruction must be a dw")?;

            let target = (self.address & !0xFFFF) + Addr::from(instruction);
            let label = labels
                .get_label_exact(target)
                .ok_or_else(|| format!("Undefined instruction ${target:06X}"))?;
            let prototype = match &label.label_type {
                LabelType::Subroutine => &default_prototype,
                LabelType::Instruction(p) => p,
                _ => {
                    return Err(format!(
                        "Instruction label ${target:06X} isn't subroutine or instruction"
                    ))
                }
            };

            for _param in &prototype.params {
                let _arg = it.next().ok_or("missing argument")?;
            }
            result.push("; instruction".into());
        }

        Ok(result)
    }

    pub fn to_string(&self, config: &Config, labels: &LabelMap) -> String {
        let mut cur_pc = self.address;
        let mut output_lines = Vec::new();

        match self.special_type {
            Some(SpecialParsingType::Spritemap) => {
                output_lines = self.generate_spritemap().expect("invalid spritemap data");
            }
            Some(SpecialParsingType::SpritemapRaw) => {
                output_lines = self
                    .generate_raw_spritemap()
                    .expect("invalid spritemap data");
            }
            Some(SpecialParsingType::InstructionList) => {
                output_lines = self
                    .generate_instruction_list(labels)
                    .expect("invalid instruction list data");
                output_lines.push(
                    Data {
                        special_type: None,
                        ..self.clone()
                    }
                    .to_string(config, labels),
                );
            }
            Some(_) => unimplemented!(),
            None => {
                let mut output = String::new();
                let mut last_data_cmd = "";
                let mut first_cmd = true;
                let mut first_val = true;

                for d in &self.data {
                    let data_cmd = d.mnemonic();
                    let data_len = d.length();

                    if !first_cmd {
                        if let Some(lbl) = labels.get_label_exact(cur_pc) {
                            /* There's a label for this address, add it into the data */
                            write!(&mut output, " : {}: ", lbl.name()).unwrap();
                            lbl.assigned.set(true);
                            first_cmd = true;
                            first_val = true;
                            last_data_cmd = "";
                        }
                    }

                    if data_cmd != last_data_cmd {
                        if !first_cmd {
                            output.push_str(" : ");
                        }
                        output.push_str(data_cmd);
                        output.push(' ');

                        last_data_cmd = data_cmd;
                        first_cmd = false;
                        first_val = true;
                    }

                    if !first_val {
                        output.push(',');
                    }

                    self.emit_data_atom(config, labels, cur_pc, &mut output, *d);

                    first_val = false;
                    cur_pc += data_len;
                }

                output_lines.push(output);
            }
        }

        let mut output = String::new();

        let mut it = output_lines.iter_mut();
        if let Some(line0) = it.next() {
            output = format!("    {:<39} ; ${:06X} |", line0, self.address);
            if let Some(comment) = &self.comment {
                write!(output, " {comment}").unwrap();
            }
        }
        for line in it {
            write!(output, "\n    {line:<40}").unwrap();
        }

        output
    }

    fn emit_data_atom(
        &self,
        config: &Config,
        labels: &LabelMap,
        cur_pc: Addr,
        output: &mut String,
        d: DataVal,
    ) {
        let override_ = config.get_override(cur_pc);
        let (type_, label_addr) = get_data_label_address(cur_pc, d, override_);
        let target = d.as_u64();

        let base = match type_ {
            OperandType::Literal => Some(LabelOrLiteral::Literal(label_addr)),
            OperandType::Address if target == 0 => Some(LabelOrLiteral::Literal(label_addr)),
            OperandType::Address | OperandType::NonNullAddress => (|| {
                Some(LabelOrLiteral::Label(
                    labels.get_label(label_addr)?.attempt_use(self.address)?,
                ))
            })(),
        };

        format_address_expression(output, target, base, d.length() as u8).unwrap();
    }
}

pub fn get_data_label_address(
    cur_pc: Addr,
    d: DataVal,
    override_: Option<&Override>,
) -> (OperandType, Addr) {
    let ov_db = override_.and_then(|o| o.db);
    let ov_type = override_.and_then(|o| o.operand_type);
    let ov_label_addr = override_.and_then(|o| o.label_addr);

    let type_ = ov_type.unwrap_or_else(|| {
        if ov_db.is_some() || ov_label_addr.is_some() || matches!(d, DataVal::DL(_)) {
            OperandType::Address
        } else {
            OperandType::Literal
        }
    });

    let (pc_bank, _) = split_addr16(cur_pc);
    let label_addr = ov_label_addr.unwrap_or_else(|| {
        let default_bank = ov_db.unwrap_or(pc_bank);
        match d {
            DataVal::DL(val) => Addr::from(val),
            DataVal::DW(val) => addr16_with_bank(default_bank, val),
            DataVal::DB(val) => addr16_with_bank(default_bank, u16::from(val)),
        }
    });

    (type_, label_addr)
}
