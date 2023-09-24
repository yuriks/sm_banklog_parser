use winnow::ascii::space0;
use winnow::combinator::{alt, delimited, separated0};
use winnow::prelude::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum InstructionParameter {
    Byte,
    Word,
    Addr,
    LongAddr,
    VramAddr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstructionPrototype {
    pub params: Vec<InstructionParameter>,
}

pub fn parse_instruction_prototype(s: &str) -> Result<Vec<InstructionParameter>, String> {
    let params = parse_instruction_prototype_inner
        .parse(s)
        .map_err(|e| e.to_string())?;
    Ok(params)
}

fn parse_instruction_prototype_inner(i: &mut &str) -> PResult<Vec<InstructionParameter>> {
    delimited(
        delimited(space0, '(', space0),
        separated0(
            alt((
                "byte".value(InstructionParameter::Byte),
                "word".value(InstructionParameter::Word),
                "addr".value(InstructionParameter::Addr),
                "long_addr".value(InstructionParameter::LongAddr),
                "vram_addr".value(InstructionParameter::VramAddr),
            )),
            delimited(space0, ',', space0),
        ),
        delimited(space0, ')', space0),
    )
    .parse_next(i)
}
