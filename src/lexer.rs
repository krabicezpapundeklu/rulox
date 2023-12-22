use std::iter::from_fn;

#[derive(Clone, Copy, Debug)]
pub struct Token {
    kind: TokenKind,
    length: u32,
}

impl Token {
    const fn new(kind: TokenKind, length: u32) -> Self {
        Self { kind, length }
    }
}

#[allow(clippy::upper_case_acronyms)]
#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenKind {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    IDENTIFIER,
    STRING { terminated: bool },
    NUMBER,

    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    COMMENT,
    WHITESPACE,
    UNKNOWN,
    EOF,
}

impl TokenKind {
    pub const fn is_multiline_token(self) -> bool {
        matches!(self, Self::STRING { .. } | Self::WHITESPACE)
    }

    pub const fn is_trivia_token(self) -> bool {
        matches!(self, Self::COMMENT | Self::WHITESPACE)
    }
}

struct Lexer<Chars> {
    chars: Chars,
    text: String,
}

impl<Chars> Lexer<Chars>
where
    Chars: Iterator<Item = char> + Clone,
{
    fn advance_token(&mut self) -> Token {
        self.text.clear();

        let kind = self.bump().map_or(TokenKind::EOF, |c| match c {
            '(' => TokenKind::LEFT_PAREN,
            ')' => TokenKind::RIGHT_PAREN,
            '{' => TokenKind::LEFT_BRACE,
            '}' => TokenKind::RIGHT_BRACE,
            ',' => TokenKind::COMMA,
            '.' => TokenKind::DOT,
            '-' => TokenKind::MINUS,
            '+' => TokenKind::PLUS,
            ';' => TokenKind::SEMICOLON,
            '*' => TokenKind::STAR,
            '/' => self.match_next('/', Self::comment, TokenKind::SLASH),
            '!' => self.match_next('=', |_| TokenKind::BANG_EQUAL, TokenKind::BANG),
            '=' => self.match_next('=', |_| TokenKind::EQUAL_EQUAL, TokenKind::EQUAL),
            '<' => self.match_next('=', |_| TokenKind::LESS_EQUAL, TokenKind::LESS),
            '>' => self.match_next('=', |_| TokenKind::GREATER_EQUAL, TokenKind::GREATER),
            '"' => self.string(),
            c if is_digit(c) => self.number(),
            c if is_alpha(c) => self.identifier(),
            c if is_whitespace(c) => self.whitespace(),
            _ => TokenKind::UNKNOWN,
        });

        Token::new(
            kind,
            u32::try_from(self.text.len()).expect("token too long"),
        )
    }

    fn bump(&mut self) -> Option<char> {
        if let Some(c) = self.chars.next() {
            self.text.push(c);
            Some(c)
        } else {
            None
        }
    }

    fn comment(&mut self) -> TokenKind {
        self.eat_while(|c| c != '\n');
        TokenKind::COMMENT
    }

    fn eat_while(&mut self, predicate: impl Fn(char) -> bool) {
        let mut chars = self.chars.clone();

        while matches!(chars.next(), Some(c) if predicate(c)) {
            self.bump();
        }
    }

    fn identifier(&mut self) -> TokenKind {
        self.eat_while(is_alphanumeric);

        match self.text.as_str() {
            "and" => TokenKind::AND,
            "class" => TokenKind::CLASS,
            "else" => TokenKind::ELSE,
            "false" => TokenKind::FALSE,
            "for" => TokenKind::FOR,
            "fun" => TokenKind::FUN,
            "if" => TokenKind::IF,
            "nil" => TokenKind::NIL,
            "or" => TokenKind::OR,
            "print" => TokenKind::PRINT,
            "return" => TokenKind::RETURN,
            "super" => TokenKind::SUPER,
            "this" => TokenKind::THIS,
            "true" => TokenKind::TRUE,
            "var" => TokenKind::VAR,
            "while" => TokenKind::WHILE,
            _ => TokenKind::IDENTIFIER,
        }
    }

    fn match_next(
        &mut self,
        expected: char,
        matches: impl Fn(&mut Self) -> TokenKind,
        does_not_match: TokenKind,
    ) -> TokenKind {
        if self.peek() == expected {
            self.bump();
            matches(self)
        } else {
            does_not_match
        }
    }

    fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or_default()
    }

    fn new(chars: Chars) -> Self {
        Self {
            chars,
            text: String::new(),
        }
    }

    fn number(&mut self) -> TokenKind {
        self.eat_while(is_digit);

        let mut chars = self.chars.clone();

        if matches!(chars.next(), Some('.')) && matches!(chars.next(), Some(c) if is_digit(c)) {
            self.bump();
            self.eat_while(is_digit);
        }

        TokenKind::NUMBER
    }

    fn string(&mut self) -> TokenKind {
        while let Some(c) = self.bump() {
            if c == '"' {
                return TokenKind::STRING { terminated: true };
            }
        }

        TokenKind::STRING { terminated: false }
    }

    fn whitespace(&mut self) -> TokenKind {
        self.eat_while(is_whitespace);
        TokenKind::WHITESPACE
    }
}

const fn is_alpha(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic()
}

const fn is_alphanumeric(c: char) -> bool {
    c == '_' || c.is_ascii_alphanumeric()
}

const fn is_digit(c: char) -> bool {
    c.is_ascii_digit()
}

const fn is_whitespace(c: char) -> bool {
    c.is_ascii_whitespace()
}

pub fn tokenize(input: &str) -> impl Iterator<Item = Token> + '_ {
    let mut lexer = Lexer::new(input.chars());
    let mut has_eof = false;

    from_fn(move || {
        if has_eof {
            return None;
        }

        let token = lexer.advance_token();

        if token.kind == TokenKind::EOF {
            has_eof = true;
        }

        Some(token)
    })
}

pub fn tokenize_with_text(input: &str) -> impl Iterator<Item = (TokenKind, &str)> {
    let tokens = tokenize(input);
    let mut position = 0;

    tokens.map(move |token| {
        let length = token.length as usize;
        let text = &input[position..position + length];

        position += length;

        (token.kind, text)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn comments() {
        let mut tokens = tokenize_with_text("//\n// comment\n///");

        assert_eq!(tokens.next(), Some((TokenKind::COMMENT, "//")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, "\n")));
        assert_eq!(tokens.next(), Some((TokenKind::COMMENT, "// comment")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, "\n")));
        assert_eq!(tokens.next(), Some((TokenKind::COMMENT, "///")));
        assert_eq!(tokens.next(), Some((TokenKind::EOF, "")));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn empty() {
        let mut tokens = tokenize_with_text("");

        assert_eq!(tokens.next(), Some((TokenKind::EOF, "")));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn identifiers() {
        let mut tokens = tokenize_with_text(
            "andy formless fo _ _123 _abc ab123\nabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"
        );

        assert_eq!(tokens.next(), Some((TokenKind::IDENTIFIER, "andy")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::IDENTIFIER, "formless")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::IDENTIFIER, "fo")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::IDENTIFIER, "_")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::IDENTIFIER, "_123")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::IDENTIFIER, "_abc")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::IDENTIFIER, "ab123")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, "\n")));

        assert_eq!(
            tokens.next(),
            Some((
                TokenKind::IDENTIFIER,
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"
            ))
        );

        assert_eq!(tokens.next(), Some((TokenKind::EOF, "")));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn keywords() {
        let mut tokens = tokenize_with_text(
            "and class else false for fun if nil or return super this true var while",
        );

        assert_eq!(tokens.next(), Some((TokenKind::AND, "and")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::CLASS, "class")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::ELSE, "else")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::FALSE, "false")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::FOR, "for")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::FUN, "fun")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::IF, "if")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::NIL, "nil")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::OR, "or")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::RETURN, "return")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::SUPER, "super")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::THIS, "this")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::TRUE, "true")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::VAR, "var")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::WHILE, "while")));
        assert_eq!(tokens.next(), Some((TokenKind::EOF, "")));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn numbers() {
        let mut tokens = tokenize_with_text("123 123.456 .456 123.");

        assert_eq!(tokens.next(), Some((TokenKind::NUMBER, "123")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::NUMBER, "123.456")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::DOT, ".")));
        assert_eq!(tokens.next(), Some((TokenKind::NUMBER, "456")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::NUMBER, "123")));
        assert_eq!(tokens.next(), Some((TokenKind::DOT, ".")));
        assert_eq!(tokens.next(), Some((TokenKind::EOF, "")));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn punctuators() {
        let mut tokens = tokenize_with_text("(){};,+-*!===<=>=!=<>/.");

        assert_eq!(tokens.next(), Some((TokenKind::LEFT_PAREN, "(")));
        assert_eq!(tokens.next(), Some((TokenKind::RIGHT_PAREN, ")")));
        assert_eq!(tokens.next(), Some((TokenKind::LEFT_BRACE, "{")));
        assert_eq!(tokens.next(), Some((TokenKind::RIGHT_BRACE, "}")));
        assert_eq!(tokens.next(), Some((TokenKind::SEMICOLON, ";")));
        assert_eq!(tokens.next(), Some((TokenKind::COMMA, ",")));
        assert_eq!(tokens.next(), Some((TokenKind::PLUS, "+")));
        assert_eq!(tokens.next(), Some((TokenKind::MINUS, "-")));
        assert_eq!(tokens.next(), Some((TokenKind::STAR, "*")));
        assert_eq!(tokens.next(), Some((TokenKind::BANG_EQUAL, "!=")));
        assert_eq!(tokens.next(), Some((TokenKind::EQUAL_EQUAL, "==")));
        assert_eq!(tokens.next(), Some((TokenKind::LESS_EQUAL, "<=")));
        assert_eq!(tokens.next(), Some((TokenKind::GREATER_EQUAL, ">=")));
        assert_eq!(tokens.next(), Some((TokenKind::BANG_EQUAL, "!=")));
        assert_eq!(tokens.next(), Some((TokenKind::LESS, "<")));
        assert_eq!(tokens.next(), Some((TokenKind::GREATER, ">")));
        assert_eq!(tokens.next(), Some((TokenKind::SLASH, "/")));
        assert_eq!(tokens.next(), Some((TokenKind::DOT, ".")));
        assert_eq!(tokens.next(), Some((TokenKind::EOF, "")));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn strings() {
        let mut tokens =
            tokenize_with_text(r#""" "stringěščřžýáíé" "this string has no close quote"#);

        assert_eq!(
            tokens.next(),
            Some((TokenKind::STRING { terminated: true }, r#""""#))
        );

        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));

        assert_eq!(
            tokens.next(),
            Some((
                TokenKind::STRING { terminated: true },
                r#""stringěščřžýáíé""#
            ))
        );

        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, " ")));

        assert_eq!(
            tokens.next(),
            Some((
                TokenKind::STRING { terminated: false },
                r#""this string has no close quote"#
            ))
        );

        assert_eq!(tokens.next(), Some((TokenKind::EOF, "")));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn unknown() {
        let mut tokens = tokenize_with_text("knowněšknown$");

        assert_eq!(tokens.next(), Some((TokenKind::IDENTIFIER, "known")));
        assert_eq!(tokens.next(), Some((TokenKind::UNKNOWN, "ě")));
        assert_eq!(tokens.next(), Some((TokenKind::UNKNOWN, "š")));
        assert_eq!(tokens.next(), Some((TokenKind::IDENTIFIER, "known")));
        assert_eq!(tokens.next(), Some((TokenKind::UNKNOWN, "$")));
        assert_eq!(tokens.next(), Some((TokenKind::EOF, "")));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn whitespace() {
        let mut tokens = tokenize_with_text("space    tabs\t\t\t\tnewlines\n\n\n\nend");

        assert_eq!(tokens.next(), Some((TokenKind::IDENTIFIER, "space")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, "    ")));
        assert_eq!(tokens.next(), Some((TokenKind::IDENTIFIER, "tabs")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, "\t\t\t\t")));
        assert_eq!(tokens.next(), Some((TokenKind::IDENTIFIER, "newlines")));
        assert_eq!(tokens.next(), Some((TokenKind::WHITESPACE, "\n\n\n\n")));
        assert_eq!(tokens.next(), Some((TokenKind::IDENTIFIER, "end")));
        assert_eq!(tokens.next(), Some((TokenKind::EOF, "")));
        assert_eq!(tokens.next(), None);
    }
}
