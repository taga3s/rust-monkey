use std::collections::HashMap;
use std::sync::LazyLock;

pub type TokenType = &'static str;

#[derive(Debug, Clone)]
pub struct Token {
    pub type_: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new() -> Self {
        Token {
            type_: ILLEGAL,
            literal: String::new(),
        }
    }
}

pub const ILLEGAL: &str = "ILLEGAL";
pub const EOF: &str = "EOF";

pub const IDENT: &str = "IDENT";
pub const INT: &str = "INT";
pub const STRING: &str = "STRING";

// operators
pub const ASSIGN: &str = "=";
pub const PLUS: &str = "+";
pub const MINUS: &str = "-";
pub const BANG: &str = "!";
pub const ASTERISK: &str = "*";
pub const SLASH: &str = "/";
pub const LT: &str = "<";
pub const GT: &str = ">";
pub const EQ: &str = "==";
pub const NOTEQ: &str = "!=";

// delimiters
pub const COMMA: &str = ",";
pub const SEMICOLON: &str = ";";
pub const COLON: &str = ":";

pub const LPAREN: &str = "(";
pub const RPAREN: &str = ")";
pub const LBRACE: &str = "{";
pub const RBRACE: &str = "}";
pub const LBRACKET: &str = "[";
pub const RBRACKET: &str = "]";

// keywords
pub const FUNCTION: &str = "FUNCTION";
pub const LET: &str = "LET";
pub const TRUE: &str = "TRUE";
pub const FALSE: &str = "FALSE";
pub const IF: &str = "IF";
pub const ELSE: &str = "ELSE";
pub const RETURN: &str = "RETURN";

pub const MACRO: &str = "MACRO";

const KEYWORDS: LazyLock<HashMap<&'static str, TokenType>> = LazyLock::new(|| {
    HashMap::from([
        ("fn", FUNCTION),
        ("let", LET),
        ("true", TRUE),
        ("false", FALSE),
        ("if", IF),
        ("else", ELSE),
        ("return", RETURN),
        ("macro", MACRO),
    ])
});

pub fn lookup_ident(ident: &str) -> TokenType {
    if let Some(&token_type) = KEYWORDS.get(ident) {
        return token_type;
    }

    IDENT
}
