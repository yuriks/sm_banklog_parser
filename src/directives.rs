use nom::{Finish, IResult};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::space0;
use nom::combinator::value;
use nom::error::{convert_error, VerboseError};
use nom::multi::separated_list0;
use nom::sequence::delimited;

#[derive(Debug, Copy, Clone)]
pub enum InstructionParameter {
    Byte, Word, Addr, LongAddr, VramAddr,
}

#[derive(Debug, Clone)]
pub struct InstructionPrototype {
    pub params: Vec<InstructionParameter>,
}

pub fn parse_instruction_prototype(s: &str) -> Result<Vec<InstructionParameter>, String> {
    let (_, params) = parse_instruction_prototype_inner(s).finish().map_err(|e| convert_error(s, e))?;
    Ok(params)
}

fn parse_instruction_prototype_inner(i: &str) -> IResult<&str, Vec<InstructionParameter>, VerboseError<&str>> {
    delimited(
        delimited(space0, tag("("), space0),
        separated_list0(
            delimited(space0, tag(","), space0),
            alt((
                value(InstructionParameter::Byte, tag("byte")),
                value(InstructionParameter::Word, tag("word")),
                value(InstructionParameter::Addr, tag("addr")),
                value(InstructionParameter::LongAddr, tag("long_addr")),
                value(InstructionParameter::VramAddr, tag("vram_addr")),
            ))
        ),
        delimited(space0, tag(")"), space0),
    )(i)
}