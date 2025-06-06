#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    StringLiteral(String),
    NumberLiteral(String),
    Plus,           // +
    PlusAssign,     // +=
    Minus,          // -
    MinusAssign,    // -=
    Star,           // *
    Ampersand,      // & // for dereferencing and bitwise AND
    StarAssign,     // *=
    Slash,          // /
    SlashAssign,    // /=
    Percent,        // %
    PercentAssign,  // %=
    LParen, RParen, LBrace, RBrace, // (, ), {, }
    Comma,
    Assign,         // '='
    SemiColon,      // ';'
    Less,           // <
    LessEqual,      // <=
    Greater,        // >
    GreaterEqual,   // >=
    EqualEqual,     // ==
    BangEqual,      // !=
    AndAnd,         // &&
    OrOr,           // ||
    Arrow,          // ->
    EOF,
}