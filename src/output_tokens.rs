use std::fmt;

use serde::Serialize;

use crate::Addr;

#[derive(Clone, Serialize)]
#[serde(tag = "t_type")]
pub enum TokenType {
    Comment,
    InstrMnemonic,
    Label,
    Literal,
    AddressExpression { target: Addr },
}

#[derive(Clone, Serialize)]
pub struct Token {
    start: usize,
    end: usize,

    #[serde(flatten)]
    t_type: TokenType,
}

pub struct TokenString {
    pub text: String,
    pub tokens: Vec<Token>,
}

pub struct TokenWriter {
    text: String,
    tokens: Vec<Token>,
    unclosed_stack: Vec<usize>,
}

impl TokenWriter {
    pub fn new() -> TokenWriter {
        TokenWriter {
            text: String::new(),
            tokens: Vec::new(),
            unclosed_stack: Vec::new(),
        }
    }

    pub fn finish(&mut self) -> TokenString {
        assert!(self.unclosed_stack.is_empty());
        let text = self.text.clone();
        let tokens = self.tokens.clone();
        self.text.clear();
        self.tokens.clear();

        TokenString { text, tokens }
    }

    pub fn open(&mut self, t_type: TokenType) {
        self.unclosed_stack.push(self.tokens.len());
        self.tokens.push(Token {
            start: self.text.len(),
            end: 0,
            t_type,
        });
    }

    pub fn close(&mut self) {
        let i = self.unclosed_stack.pop().unwrap();
        self.tokens[i].end = self.text.len();
    }
}

impl fmt::Write for TokenWriter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.text.write_str(s)
    }
}

impl Into<TokenString> for TokenWriter {
    fn into(self) -> TokenString {
        assert!(self.unclosed_stack.is_empty());
        TokenString {
            text: self.text,
            tokens: self.tokens,
        }
    }
}
