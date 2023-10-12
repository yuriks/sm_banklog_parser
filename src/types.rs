use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;

use serde::Deserialize;

pub type Addr = u32;
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Deserialize)]
#[serde(transparent)]
pub struct Bank(pub u8);

impl Bank {
    pub fn addr(self, low_addr: u16) -> Addr {
        (Addr::from(self.0) << 16) + Addr::from(low_addr)
    }

    pub fn of(addr: Addr) -> Bank {
        Bank((addr >> 16) as u8)
    }
}

impl Display for Bank {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "${:02X}", self.0)
    }
}

impl Debug for Bank {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Bank({self})")
    }
}

impl fmt::UpperHex for Bank {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:02X}", self.0)
    }
}

impl fmt::LowerHex for Bank {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:02x}", self.0)
    }
}

impl From<u8> for Bank {
    fn from(bank: u8) -> Bank {
        Bank(bank)
    }
}

impl FromStr for Bank {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let no_prefix = s
            .strip_prefix('$')
            .or_else(|| s.strip_prefix("0x"))
            .unwrap_or(s);
        u8::from_str_radix(no_prefix, 16).map(Bank)
    }
}
