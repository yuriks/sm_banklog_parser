use crate::config::{Config, OperandType, Override, OverrideAddr};
use crate::label::{LabelMap, LabelType};

pub fn generate_overrides(config: &mut Config, labels: &LabelMap) {
    let mut new_overrides = Vec::new();
    for label in labels.iter_labels() {
        let (name, is_table) = match &label.label_type {
            LabelType::Struct { struct_ } => (struct_, false),
            LabelType::StructTable { struct_ } => (struct_, true),
            _ => continue,
        };

        let struct_ = config.get_struct(name).unwrap();

        let mut length = label.length;
        if length == 0 {
            if is_table {
                panic!("Unsized StructTable label {}", label.name());
            } else {
                length = struct_.size();
            }
        }

        if is_table {
            if length % struct_.size() != 0 {
                eprintln!(
                    "StructTable label length (${:X}) is not multiple of Struct {} size (${:X})",
                    length,
                    struct_.name,
                    struct_.size()
                );
            }
        } else if length != struct_.size() {
            eprintln!(
                "Struct label length (${:X}) does not match Struct {} size (${:X})",
                length,
                struct_.name,
                struct_.size()
            );
        }

        for field in &struct_.fields {
            let operand_type = match field.type_ {
                LabelType::Undefined
                | LabelType::Blocked
                | LabelType::Subroutine
                | LabelType::SubroutineLong
                | LabelType::Branch
                | LabelType::CodeLocation
                | LabelType::Instruction(_) => panic!("Invalid struct field"),
                LabelType::Data | LabelType::DataTable => OperandType::Literal,
                LabelType::Pointer | LabelType::PointerTable => OperandType::Address,
                LabelType::Struct { .. } | LabelType::StructTable { .. } => {
                    unimplemented!("Recursive struct")
                }
            };

            for struct_offset in (0..length).step_by(struct_.size() as usize) {
                let field_base = label.address + struct_offset + field.offset;
                let ov = Override {
                    db: field.db,
                    operand_type: Some(operand_type),
                    label_name_hint: Some(field.name.clone()),
                    struct_: None,
                    ..Override::new(OverrideAddr::Range(
                        field_base,
                        field_base + field.length - 1,
                    ))
                };
                new_overrides.push(ov);
            }
        }
    }

    config.add_overrides(new_overrides);
}
