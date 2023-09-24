use crate::data::DataVal;
use crate::{addr16_with_bank, Addr};
use winnow::ascii::{hex_uint, space0, space1};
use winnow::combinator::{
    alt, cut_err, delimited, iterator, opt, preceded, separated_pair, terminated,
};
use winnow::error::{ErrMode, ErrorKind, ParserError};
use winnow::prelude::*;
use winnow::token::{take, take_till0};

fn hex2(i: &mut &str) -> PResult<u8> {
    take(2usize).and_then(hex_uint).parse_next(i)
}

fn hex4(i: &mut &str) -> PResult<u16> {
    take(4usize).and_then(hex_uint).parse_next(i)
}

fn line_comment<'i>(i: &mut &'i str) -> PResult<&'i str> {
    preceded(';', take_till0(['\r', '\n'])).parse_next(i)
}

fn pc_prefix(i: &mut &str) -> PResult<Addr> {
    preceded('$', separated_pair(hex2, ':', hex4))
        .map(|(bank, low_addr)| addr16_with_bank(bank, low_addr))
        .parse_next(i)
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
        // anyhow!("Data atom had {nibbles} nibbles, expected 2, 4, 6 or 8")
        _ => return Err(ErrMode::from_error_kind(i, ErrorKind::Verify)), //.add_context(i, "data atom")),
    })
}

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
                DL(0xABCDEF)
            ]
        );
        assert_eq!(res.comment, None);
    }

    #[test]
    fn test_data_line_trailing_spaces() {
        use DataVal::{DB, DL, DW};

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
}
