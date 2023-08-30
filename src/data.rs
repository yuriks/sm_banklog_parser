use std::fmt::Write;

use if_chain::if_chain;

use crate::config::Config;
use crate::label::LabelMap;
use crate::SpecialParsingType;

#[derive(Debug, Clone, Copy)]
pub enum DataVal {
    DB(u8),
    DW(u16),
    DL(u32),
}

impl DataVal {
    pub fn get_db(&self) -> Option<u8> {
        match *self {
            DataVal::DB(x) => Some(x),
            _ => None,
        }
    }

    pub fn get_dw(&self) -> Option<u16> {
        match *self {
            DataVal::DW(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_u64(&self) -> u64 {
        match self {
            DataVal::DB(b) => *b as u64,
            DataVal::DW(w) => *w as u64,
            DataVal::DL(l) => *l as u64
        }
    }

    pub fn length(&self) -> u64 {
        match self {
            DataVal::DB(_) => 1,
            DataVal::DW(_) => 2,
            DataVal::DL(_) => 3,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Data {
    pub address: u64,
    pub data: Vec<DataVal>,
    pub comment: Option<String>,
    pub special_type: Option<SpecialParsingType>,
}

impl Data {
    pub fn generate_spritemap(&self) -> Result<Vec<String>, String> {
        let mut result = Vec::new();
        let mut it = self.data.iter();

        let count = it.next().and_then(|d| d.get_dw()).ok_or("expected dw count")?;
        result.push(format!("dw ${:04X}", count));

        for _ in 0..count {
            let val1 = it.next().and_then(|d| d.get_dw()).ok_or("expected dw value 1")?;
            let val2 = it.next().and_then(|d| d.get_db()).ok_or("expected db value 2")?;
            let val3 = it.next().and_then(|d| d.get_dw()).ok_or("expected dw value 3")?;

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

            write!(l, "%spritemap_entry({:3}, {:3}, ${:03X}, ", val_x, val_y, val_tile).unwrap();

            let mut wrote_flag = false;
            macro_rules! add_and {
                () => {
                    if wrote_flag {
                       l.push_str("|");
                    }
                    wrote_flag = true;
                }
            }

            if val_pal != 0 {
                wrote_flag = true;
                write!(l, "SPRM_PAL({})", val_pal).unwrap();
            }

            if val_prio != 0 {
                add_and!();
                write!(l, "SPRM_PRIO({})", val_prio).unwrap();
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
                l.push_str("0");
            }

            l.push_str(")");
        }

        if it.next().is_some() {
            return Err("trailing data".into());
        }

        Ok(result)
    }

    pub fn generate_raw_spritemap(&self) -> Result<Vec<String>, String> {
        let mut result = Vec::new();
        let mut it = self.data.iter();

        let count = it.next().and_then(|d| d.get_dw()).ok_or("expected dw count")?;
        result.push(format!("dw ${:04X}", count));

        for i in 0..count {
            let val1 = it.next().and_then(|d| d.get_dw()).ok_or("expected dw value 1")?;
            let val2 = it.next().and_then(|d| d.get_db()).ok_or("expected db value 2")?;
            let val3 = it.next().and_then(|d| d.get_dw()).ok_or("expected dw value 3")?;

            // Write 4 entries per line
            let new_line = (count > 2) && (i % 2 == 0);
            if new_line {
                result.push(String::new());
            }
            let l = result.last_mut().unwrap();
            if !new_line {
                l.push_str(" : ");
            }

            write!(l, "%spritemap_raw(${:04X}, ${:02X}, ${:04X})", val1, val2, val3).unwrap();
        }

        if it.next().is_some() {
            return Err("trailing data".into());
        }

        Ok(result)
    }

    pub fn to_string(&self, config: &Config, labels: &mut LabelMap) -> String {
        let mut cur_pc = self.address;
        let mut output_lines = Vec::new();

        match self.special_type {
            Some(SpecialParsingType::Spritemap) => {
                output_lines = self.generate_spritemap().expect("invalid spritemap data");
            },
            Some(SpecialParsingType::SpritemapRaw) => {
                output_lines = self.generate_raw_spritemap().expect("invalid spritemap data");
            }
            Some(_) => unimplemented!(),
            None => {
                let mut output = String::new();
                let mut last_data_cmd = "";
                let mut first_cmd = true;
                let mut first_val = true;

                for d in &self.data {
                    let (data_cmd, data_len) = match d {
                        DataVal::DB(_) => ("db", 1),
                        DataVal::DW(_) => ("dw", 2),
                        DataVal::DL(_) => ("dl", 3)
                    };

                    if !first_cmd && labels.contains_key(&cur_pc) {
                        /* There's a label for this address, add it into the data */
                        output.push_str(&format!(" : {}: ", labels[&cur_pc].name));
                        let lbl = labels.get_mut(&cur_pc).unwrap();
                        lbl.assigned = true;
                        first_cmd = true;
                        first_val = true;
                        last_data_cmd = "";
                    }

                    if data_cmd != last_data_cmd {
                        output.push_str(&format!("{}{} ", if first_cmd { "" } else { " : " }, data_cmd));
                        last_data_cmd = data_cmd;
                        first_val = true;
                        first_cmd = false;
                    }

                    if_chain! {
                        if let DataVal::DL(dl) = d;
                        if labels.contains_key(&(*dl as u64));
                        then {
                            output.push_str(&format!("{}{}", if first_val { "" } else { "," }, labels[&(*dl as u64)].name));
                        } else {
                            if_chain! {
                                if let Some(ov) = config.get_override(cur_pc);
                                if let Some(t) = &ov._type;
                                if t == "Pointer" || t == "Data";
                                then {
                                    let db = ov.db.unwrap_or(cur_pc >> 16);
                                    let label_addr = (d.as_u64() & 0xFFFF_u64) | (db << 16);
                                    if labels.contains_key(&label_addr) {
                                        output.push_str(&format!("{}{}", if first_val { "" } else { "," }, labels[&label_addr].name));
                                    } else {
                                        match d {
                                            DataVal::DB(db) => output.push_str(&format!("{}${:02X}", if first_val { "" } else { "," }, db)),
                                            DataVal::DW(dw) => output.push_str(&format!("{}${:04X}", if first_val { "" } else { "," }, dw)),
                                            DataVal::DL(dl) => output.push_str(&format!("{}${:06X}", if first_val { "" } else { "," }, dl)),
                                        }
                                    }
                                } else {
                                    if_chain! {
                                        if let Some(ov) = config.get_override(cur_pc);
                                        if let Some(t) = &ov._type;
                                        if t == "Struct";
                                        if let Some(st) = config.structs.iter().find(|s| &s.name == ov._struct.as_ref().unwrap_or(&"".to_string()));
                                        then {
                                            let last_field = &st.fields[st.fields.len() - 1];
                                            let st_len = last_field.offset + last_field.length;
                                            let cur_offset = cur_pc - self.address;
                                            let cur_st_offset = cur_offset % st_len;
                                            let field = &st.fields.iter().find(|f| f.offset == cur_st_offset).unwrap();
                                            let db = field.db.unwrap_or(cur_pc >> 16);
                                            let label_addr = if field.length < 3 { (d.as_u64() & 0xFFFF_u64) | (db << 16) } else { d.as_u64() };
                                            if field._type == "Pointer" && (label_addr & 0xFFFF_u64) >= 0x8000 && labels.contains_key(&label_addr) {
                                                output.push_str(&format!("{}{}", if first_val { "" } else { "," }, labels[&label_addr].name));
                                            } else {
                                                match d {
                                                    DataVal::DB(db) => output.push_str(&format!("{}${:02X}", if first_val { "" } else { "," }, db)),
                                                    DataVal::DW(dw) => output.push_str(&format!("{}${:04X}", if first_val { "" } else { "," }, dw)),
                                                    DataVal::DL(dl) => output.push_str(&format!("{}${:06X}", if first_val { "" } else { "," }, dl)),
                                                }
                                            }
                                        } else {
                                            match d {
                                                DataVal::DB(db) => output.push_str(&format!("{}${:02X}", if first_val { "" } else { "," }, db)),
                                                DataVal::DW(dw) => output.push_str(&format!("{}${:04X}", if first_val { "" } else { "," }, dw)),
                                                DataVal::DL(dl) => output.push_str(&format!("{}${:06X}", if first_val { "" } else { "," }, dl)),
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

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
                write!(output, " {}", comment).unwrap();
            }
        }
        for line in it {
            write!(output, "\n    {:<40}", line).unwrap();
        }

        output
    }    
}