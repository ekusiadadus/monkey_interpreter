use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub literal: String,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    Illegal,
    Eof,

    Ident,
    Int,

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,

    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Function,
    Let,
    If,
    Else,
    Return,

    True,
    False,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Illegal => write!(f, "Illegal"),
            TokenKind::Eof => write!(f, "Eof"),
            TokenKind::Ident => write!(f, "Ident"),
            TokenKind::Int => write!(f, "Int"),
            TokenKind::Assign => write!(f, "="),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Asterisk => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LBrace => write!(f, "{}", std::char::from_u32(123).unwrap()),
            TokenKind::RBrace => write!(f, "{}", std::char::from_u32(125).unwrap()),
            TokenKind::Function => write!(f, "fn"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Return => write!(f, "return"),
        }
    }
}

impl TokenKind {
    pub fn lookup_ident(identifier: String) -> TokenKind {
        match identifier.as_str() {
            "fn" => TokenKind::Function,
            "let" => TokenKind::Let,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "return" => TokenKind::Return,
            _ => TokenKind::Ident,
        }
    }
}
