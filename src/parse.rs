use crate::data::DataVal;
use crate::{addr16_with_bank, Addr, Bank};
use winnow::ascii::{hex_uint, space0, space1};
use winnow::combinator::{
    alt, cut_err, delimited, eof, iterator, opt, preceded, separated1, separated_pair, terminated,
};
use winnow::error::{ErrMode, ErrorKind, ParserError};
use winnow::prelude::*;
use winnow::stream::{AsChar, ContainsToken, SliceLen, Stream};
use winnow::token::{take_till0, take_while};
use winnow::trace::trace;

pub fn of_length<I: Stream, O, E: ParserError<I>, P: Parser<I, O, E>>(
    expected_len: usize,
    parser: P,
) -> impl Parser<I, O, E>
where
    I::Slice: SliceLen,
{
    trace(
        "of_length",
        parser.with_recognized().verify_map(move |(v, r)| {
            if r.slice_len() == expected_len {
                Some(v)
            } else {
                None
            }
        }),
    )
}
fn hex2(i: &mut &str) -> PResult<u8> {
    of_length(2, hex_uint).parse_next(i)
}

fn hex4(i: &mut &str) -> PResult<u16> {
    of_length(4, hex_uint).parse_next(i)
}

fn hex6(i: &mut &str) -> PResult<u32> {
    of_length(6, hex_uint).parse_next(i)
}

fn line_comment<'i>(i: &mut &'i str) -> PResult<&'i str> {
    preceded(';', take_till0(['\r', '\n'])).parse_next(i)
}

fn pc_prefix(i: &mut &str) -> PResult<Addr> {
    preceded('$', separated_pair(hex2, ':', hex4))
        .map(|(bank, low_addr)| addr16_with_bank(bank, low_addr))
        .parse_next(i)
}

fn pc_address(i: &mut &str) -> PResult<(Option<Bank>, u16)> {
    preceded('$', (opt(terminated(hex2, ':')), hex4)).parse_next(i)
}

// Present after `pc_prefix` in banks containing SPC code
fn spc_pc_prefix(i: &mut &str) -> PResult<u16> {
    preceded('/', cut_err(preceded('$', hex4))).parse_next(i)
}

fn data_atom(i: &mut &str) -> PResult<impl Iterator<Item = DataVal>> {
    use std::iter::once;

    let (atom, atom_str) = hex_uint.with_recognized().parse_next(i)?;
    let nibbles = atom_str.len();

    Ok(match nibbles {
        2 => once(DataVal::DB(atom as u8)).chain(None),
        4 => once(DataVal::DW(atom as u16)).chain(None),
        6 => once(DataVal::DL(atom)).chain(None),
        8 => {
            let a = DataVal::DW(atom as u16);
            let b = DataVal::DW((atom >> 16) as u16);
            once(a).chain(Some(b))
        }
        _ => return Err(ErrMode::from_error_kind(i, ErrorKind::Verify).cut()),
    })
}

#[allow(clippy::from_iter_instead_of_collect)]
fn data_list(i: &mut &str) -> PResult<Vec<DataVal>> {
    let mut data = Vec::from_iter(data_atom.parse_next(i)?);

    let mut it = iterator(*i, preceded(delimited(space0, ',', space0), data_atom));
    data.extend(it.flatten());
    *i = it.finish()?.0;

    // A trailing comma is allowed for line continuations
    opt(preceded(space0, ',')).parse_next(i)?;

    Ok(data)
}

pub struct ParsedDataLine<'i> {
    pub line_addr: Addr,
    pub data_type: &'i str,
    pub data_values: Vec<DataVal>,
    pub comment: Option<&'i str>,
}

pub fn parse_data_line<'i>(i: &mut &'i str) -> PResult<ParsedDataLine<'i>> {
    let line_addr = terminated(pc_prefix, opt(spc_pc_prefix)).parse_next(i)?;
    space1.parse_next(i)?;
    let data_type = alt(("db", "dw", "dl", "dx", "dW")).parse_next(i)?;
    space1.parse_next(i)?;
    let data_values = data_list.parse_next(i)?;
    let comment = preceded(space0, opt(line_comment)).parse_next(i)?;

    Ok(ParsedDataLine {
        line_addr,
        data_type,
        data_values,
        comment,
    })
}

pub fn parse_data_line_continuation<'i>(i: &mut &'i str) -> PResult<ParsedDataLine<'i>> {
    space1.parse_next(i)?;
    let data_values = data_list.parse_next(i)?;
    let comment = preceded(space0, opt(line_comment)).parse_next(i)?;

    Ok(ParsedDataLine {
        line_addr: 0,
        data_type: "",
        data_values,
        comment,
    })
}

pub struct ParsedCodeLine<'i> {
    pub line_addr: Addr,
    pub instruction_bytes: Vec<u8>,
    pub mnemonic: &'i str,
    pub logged_address: Option<(Option<Bank>, u16)>,
    pub comment: Option<&'i str>,
}

pub fn parse_code_line<'i>(i: &mut &'i str) -> PResult<ParsedCodeLine<'i>> {
    let line_addr = pc_prefix.parse_next(i)?;
    space1.parse_next(i)?;
    let instruction_bytes = instruction_bytes.parse_next(i)?;
    space1.parse_next(i)?;
    let mnemonic = instruction_mnemonic.parse_next(i)?;
    match mnemonic {
        "MVN" | "MVP" => {
            preceded(space1, separated_pair(hex2, ' ', hex2)).parse_next(i)?;
        }
        _ => {
            opt(preceded(space1, instruction_operand_soup)).parse_next(i)?;
        }
    }
    let logged_address = opt(preceded(space0, instruction_logged_address)).parse_next(i)?;
    let comment = preceded(space0, opt(line_comment)).parse_next(i)?;

    Ok(ParsedCodeLine {
        line_addr,
        instruction_bytes,
        mnemonic,
        logged_address,
        comment,
    })
}

fn instruction_bytes(i: &mut &str) -> PResult<Vec<u8>> {
    separated1(hex2, ' ').parse_next(i)
}

fn instruction_mnemonic<'i>(i: &mut &'i str) -> PResult<&'i str> {
    take_while(3, AsChar::is_alpha).parse_next(i)
}

fn instruction_operand_soup<'i>(i: &mut &'i str) -> PResult<&'i str> {
    let soup = || {
        take_while(
            0..,
            ('#', '$', 'A'..='F', '0'..='9', 's', 'x', 'y', ',', '(', ')'),
        )
    };

    opt(delimited('[', soup(), ']')).parse_next(i)?;
    soup().parse_next(i)
}

fn instruction_logged_address(i: &mut &str) -> PResult<(Option<Bank>, u16)> {
    delimited('[', pc_address, ']').parse_next(i)
}

pub fn parse_comment_line<'i>(i: &mut &'i str) -> PResult<&'i str> {
    preceded(space0, line_comment).parse_next(i)
}

pub fn parse_bracket_line<'i>(i: &mut &'i str) -> PResult<(char, Option<&'i str>)> {
    let bracket = preceded(space0, alt(('{', '}'))).parse_next(i)?;
    let comment = preceded(space0, opt(line_comment)).parse_next(i)?;
    Ok((bracket, comment))
}

pub struct ParsedFillToLine<'i> {
    pub line_addr: Addr,
    pub target: Addr,
    pub fill_byte: u8,
    pub comment: Option<&'i str>,
}

pub fn parse_fillto_line<'i>(i: &mut &'i str) -> PResult<ParsedFillToLine<'i>> {
    let line_addr = pc_prefix.parse_next(i)?;
    delimited(space1, "fillto", space1).parse_next(i)?;
    let (target, fill_byte) = separated_pair(
        preceded('$', hex6),
        delimited(space0, ',', space0),
        preceded('$', hex2),
    )
    .parse_next(i)?;
    let comment = preceded(space0, opt(line_comment)).parse_next(i)?;

    Ok(ParsedFillToLine {
        line_addr,
        target: Addr::from(target),
        fill_byte,
        comment,
    })
}

fn lazy_quantifier<
    I: Stream,
    O,
    E: ParserError<I>,
    P: ContainsToken<I::Token>,
    F: Parser<I, O, E>,
>(
    predicate: P,
    mut terminator: F,
) -> impl Parser<I, (I::Slice, O), E> {
    move |i: &mut I| {
        let match_start = i.checkpoint();
        loop {
            let potential_match_end = i.checkpoint();
            let match_length = i.offset_from(&match_start);
            match terminator.parse_next(i) {
                Ok(o) => {
                    let final_pos = i.checkpoint();
                    i.reset(match_start);
                    let matched = i.next_slice(match_length);
                    i.reset(final_pos);
                    return Ok((matched, o));
                }
                Err(ErrMode::Backtrack(_)) => i.reset(potential_match_end),
                Err(e) => return Err(e),
            }

            let c = i.next_token();
            if !c.is_some_and(|c| predicate.contains_token(c)) {
                return Err(ErrMode::from_error_kind(i, ErrorKind::Slice));
            }
        }
    }
}

pub fn parse_sub_comment<'i>(i: &mut &'i str) -> PResult<(u16, &'i str)> {
    let addr = delimited((";;", space1, '$'), hex4, (':', space0)).parse_next(i)?;
    let (description, ()) =
        lazy_quantifier(|_| true, (opt((space0, ";;;")), space0, eof).void()).parse_next(i)?;
    Ok((addr, description))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_data_line() {
        use DataVal::{DB, DL, DW};

        let line = "$A0:CF7F             dx 0C00, 8B60, A2, 00, 12345678, ABCDEF";
        let res = parse_data_line.parse(line);

        let res = res.unwrap();
        assert_eq!(res.line_addr, 0xA0_CF7F);
        assert_eq!(res.data_type, "dx");
        assert_eq!(
            res.data_values,
            vec![
                DW(0x0C00),
                DW(0x8B60),
                DB(0xA2),
                DB(0x00),
                DW(0x5678),
                DW(0x1234),
                DL(0xAB_CDEF),
            ]
        );
        assert_eq!(res.comment, None);
    }

    #[test]
    fn test_data_line_trailing_spaces() {
        use DataVal::DW;

        let line = "$87:8422             dx 000C,9524,       ";
        let res = parse_data_line.parse(line);

        let res = res.unwrap();
        assert_eq!(res.line_addr, 0x87_8422);
        assert_eq!(res.data_type, "dx");
        assert_eq!(res.data_values, vec![DW(0x000C), DW(0x9524)]);
        assert_eq!(res.comment, None);
    }

    #[test]
    fn test_data_line_continuation_empty_string() {
        let line = "                                            ";
        let res = parse_data_line_continuation.parse(line);

        assert!(res.is_err());
    }

    #[test]
    fn test_code_line() {
        let line = "$82:8CB2 BC 9D 1A    LDY $1A9D,x[$7E:1AAB]  ; Y = [game options menu object spritemap pointer]";
        let res = parse_code_line.parse(line);

        let res = res.unwrap();
        assert_eq!(res.line_addr, 0x82_8CB2);
        assert_eq!(res.instruction_bytes, vec![0xBC, 0x9D, 0x1A]);
        assert_eq!(res.mnemonic, "LDY");
        assert_eq!(res.logged_address, Some((Some(0x7E), 0x1AAB)));
        assert_eq!(
            res.comment,
            Some(" Y = [game options menu object spritemap pointer]")
        );
    }

    #[test]
    fn test_code_line_no_operand() {
        let line = "$80:8027 6B          RTL";
        let res = parse_code_line.parse(line);

        let res = res.unwrap();
        assert_eq!(res.line_addr, 0x80_8027);
        assert_eq!(res.instruction_bytes, vec![0x6B]);
        assert_eq!(res.mnemonic, "RTL");
        assert_eq!(res.logged_address, None);
        assert_eq!(res.comment, None);
    }

    #[test]
    fn test_code_line_indirect_long_operand() {
        let line = "$80:801B B7 03       LDA [$03],y[$80:845D]  ;|";
        let res = parse_code_line.parse(line);

        let res = res.unwrap();
        assert_eq!(res.line_addr, 0x80_801B);
        assert_eq!(res.instruction_bytes, vec![0xB7, 0x03]);
        assert_eq!(res.mnemonic, "LDA");
        assert_eq!(res.logged_address, Some((Some(0x80), 0x845D)));
        assert_eq!(res.comment, Some("|"));
    }

    #[test]
    fn test_code_line_indirect_long_jump() {
        let line = "$82:8D11 DC 01 06    JML [$0601][$88:83E1]  ;|";
        let res = parse_code_line.parse(line);

        let res = res.unwrap();
        assert_eq!(res.line_addr, 0x82_8D11);
        assert_eq!(res.instruction_bytes, vec![0xDC, 0x01, 0x06]);
        assert_eq!(res.mnemonic, "JML");
        assert_eq!(res.logged_address, Some((Some(0x88), 0x83E1)));
        assert_eq!(res.comment, Some("|"));
    }

    #[test]
    fn test_code_line_mnemonic_starts_with_hex_digit() {
        // CMP begin with 'C' which is a valid hex digit. This tests that parsing of the instruction
        // bytes will backtrack correctly.
        let line = "$80:8066 CF 40 21 00 CMP $002140[$7E:2140]  ;|";
        let res = parse_code_line.parse(line);

        let res = res.unwrap();
        assert_eq!(res.line_addr, 0x80_8066);
        assert_eq!(res.instruction_bytes, vec![0xCF, 0x40, 0x21, 0x00]);
        assert_eq!(res.mnemonic, "CMP");
        assert_eq!(res.logged_address, Some((Some(0x7E), 0x2140)));
        assert_eq!(res.comment, Some("|"));
    }

    #[test]
    fn test_lazy_quantifier() {
        fn fancy_comment<'i>(i: &mut &'i str) -> PResult<(&'i str, &'i str)> {
            preceded(
                ";;; ",
                lazy_quantifier(|_| true, (space0, ";;;", space0, eof).recognize()),
            )
            .parse_next(i)
        }

        let res = fancy_comment.parse(";;; test text ;;;");
        assert_eq!(res, Ok(("test text", " ;;;")));

        let res = fancy_comment.parse(";;; test ;;; with more semicolon ;;;  ");
        assert_eq!(res, Ok(("test ;;; with more semicolon", " ;;;  ")));
    }
}
