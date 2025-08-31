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

pub static ILLEGAL: &str = "ILLEGAL";
pub static EOF: &str = "EOF";

pub static IDENT: &str = "IDENT";
pub static INT: &str = "INT";
pub static STRING: &str = "STRING";

// operators
pub static ASSIGN: &str = "=";
pub static PLUS: &str = "+";
pub static MINUS: &str = "-";
pub static BANG: &str = "!";
pub static ASTERISK: &str = "*";
pub static SLASH: &str = "/";
pub static LT: &str = "<";
pub static GT: &str = ">";
pub static EQ: &str = "==";
pub static NOTEQ: &str = "!=";

// delimiters
pub static COMMA: &str = ",";
pub static SEMICOLON: &str = ";";
pub static COLON: &str = ":";

pub static LPAREN: &str = "(";
pub static RPAREN: &str = ")";
pub static LBRACE: &str = "{";
pub static RBRACE: &str = "}";
pub static LBRACKET: &str = "[";
pub static RBRACKET: &str = "]";

// keywords
pub static FUNCTION: &str = "FUNCTION";
pub static LET: &str = "LET";
pub static TRUE: &str = "TRUE";
pub static FALSE: &str = "FALSE";
pub static IF: &str = "IF";
pub static ELSE: &str = "ELSE";
pub static RETURN: &str = "RETURN";

pub static MACRO: &str = "MACRO";

static KEYWORDS: LazyLock<HashMap<&'static str, TokenType>> = LazyLock::new(|| {
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
