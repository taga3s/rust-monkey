// pub type TokenType = &'static str;

#[derive(Debug, Clone)]
pub struct Token {
    pub type_: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new() -> Self {
        Token {
            type_: TokenType::ILLEGAL,
            literal: String::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    STRING,
    // operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NOTEQ,
    // delimiters
    COMMA,
    SEMICOLON,
    COLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    // keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
    MACRO,
}

const KEYWORDS: [(&str, TokenType); 8] = [
    ("fn", TokenType::FUNCTION),
    ("let", TokenType::LET),
    ("true", TokenType::TRUE),
    ("false", TokenType::FALSE),
    ("if", TokenType::IF),
    ("else", TokenType::ELSE),
    ("return", TokenType::RETURN),
    ("macro", TokenType::MACRO),
];

pub fn lookup_ident(ident: &str) -> TokenType {
    if let Some((_, token_type)) = KEYWORDS.iter().find(|(key, _)| *key == ident) {
        return token_type.clone();
    }
    TokenType::IDENT
}
