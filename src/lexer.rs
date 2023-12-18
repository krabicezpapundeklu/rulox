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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,
    String { terminated: bool },
    Number,

    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Comment,
    Whitespace,
    Unknown,
    EndOfInput,
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

        let kind = self.bump().map_or(TokenKind::EndOfInput, |c| match c {
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '-' => TokenKind::Minus,
            '+' => TokenKind::Plus,
            ';' => TokenKind::Semicolon,
            '*' => TokenKind::Star,
            '/' => self.match_next('/', Self::comment, TokenKind::Slash),
            '!' => self.match_next('=', |_| TokenKind::BangEqual, TokenKind::Bang),
            '=' => self.match_next('=', |_| TokenKind::EqualEqual, TokenKind::Equal),
            '<' => self.match_next('=', |_| TokenKind::LessEqual, TokenKind::Less),
            '>' => self.match_next('=', |_| TokenKind::GreaterEqual, TokenKind::Greater),
            '"' => self.string(),
            c if is_digit(c) => self.number(),
            c if is_alpha(c) => self.identifier(),
            c if is_whitespace(c) => self.whitespace(),
            _ => TokenKind::Unknown,
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
        TokenKind::Comment
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
            "and" => TokenKind::And,
            "class" => TokenKind::Class,
            "else" => TokenKind::Else,
            "false" => TokenKind::False,
            "for" => TokenKind::For,
            "fun" => TokenKind::Fun,
            "if" => TokenKind::If,
            "nil" => TokenKind::Nil,
            "or" => TokenKind::Or,
            "print" => TokenKind::Print,
            "return" => TokenKind::Return,
            "super" => TokenKind::Super,
            "this" => TokenKind::This,
            "true" => TokenKind::True,
            "var" => TokenKind::Var,
            "while" => TokenKind::While,
            _ => TokenKind::Identifier,
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

        TokenKind::Number
    }

    fn string(&mut self) -> TokenKind {
        while let Some(c) = self.bump() {
            if c == '"' {
                return TokenKind::String { terminated: true };
            }
        }

        TokenKind::String { terminated: false }
    }

    fn whitespace(&mut self) -> TokenKind {
        self.eat_while(is_whitespace);
        TokenKind::Whitespace
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
    let mut next_token: Option<Token> = None;

    from_fn(move || {
        let mut token = next_token.map_or_else(
            || lexer.advance_token(),
            |token| {
                next_token = None;
                token
            },
        );

        match token.kind {
            TokenKind::EndOfInput => None,
            TokenKind::Unknown => {
                let mut length = 0;

                loop {
                    length += token.length;
                    token = lexer.advance_token();

                    if token.kind != TokenKind::Unknown {
                        next_token = Some(token);
                        return Some(Token::new(TokenKind::Unknown, length));
                    }
                }
            }
            _ => Some(token),
        }
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

        assert_eq!(tokens.next(), Some((TokenKind::Comment, "//")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, "\n")));
        assert_eq!(tokens.next(), Some((TokenKind::Comment, "// comment")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, "\n")));
        assert_eq!(tokens.next(), Some((TokenKind::Comment, "///")));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn empty() {
        let mut tokens = tokenize_with_text("");
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn identifiers() {
        let mut tokens = tokenize_with_text(
            "andy formless fo _ _123 _abc ab123\nabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"
        );

        assert_eq!(tokens.next(), Some((TokenKind::Identifier, "andy")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::Identifier, "formless")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::Identifier, "fo")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::Identifier, "_")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::Identifier, "_123")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::Identifier, "_abc")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::Identifier, "ab123")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, "\n")));

        assert_eq!(
            tokens.next(),
            Some((
                TokenKind::Identifier,
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"
            ))
        );

        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn keywords() {
        let mut tokens = tokenize_with_text(
            "and class else false for fun if nil or return super this true var while",
        );

        assert_eq!(tokens.next(), Some((TokenKind::And, "and")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::Class, "class")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::Else, "else")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::False, "false")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::For, "for")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::Fun, "fun")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::If, "if")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::Nil, "nil")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::Or, "or")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::Return, "return")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::Super, "super")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::This, "this")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::True, "true")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::Var, "var")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::While, "while")));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn numbers() {
        let mut tokens = tokenize_with_text("123 123.456 .456 123.");

        assert_eq!(tokens.next(), Some((TokenKind::Number, "123")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::Number, "123.456")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::Dot, ".")));
        assert_eq!(tokens.next(), Some((TokenKind::Number, "456")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));
        assert_eq!(tokens.next(), Some((TokenKind::Number, "123")));
        assert_eq!(tokens.next(), Some((TokenKind::Dot, ".")));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn punctuators() {
        let mut tokens = tokenize_with_text("(){};,+-*!===<=>=!=<>/.");

        assert_eq!(tokens.next(), Some((TokenKind::LeftParen, "(")));
        assert_eq!(tokens.next(), Some((TokenKind::RightParen, ")")));
        assert_eq!(tokens.next(), Some((TokenKind::LeftBrace, "{")));
        assert_eq!(tokens.next(), Some((TokenKind::RightBrace, "}")));
        assert_eq!(tokens.next(), Some((TokenKind::Semicolon, ";")));
        assert_eq!(tokens.next(), Some((TokenKind::Comma, ",")));
        assert_eq!(tokens.next(), Some((TokenKind::Plus, "+")));
        assert_eq!(tokens.next(), Some((TokenKind::Minus, "-")));
        assert_eq!(tokens.next(), Some((TokenKind::Star, "*")));
        assert_eq!(tokens.next(), Some((TokenKind::BangEqual, "!=")));
        assert_eq!(tokens.next(), Some((TokenKind::EqualEqual, "==")));
        assert_eq!(tokens.next(), Some((TokenKind::LessEqual, "<=")));
        assert_eq!(tokens.next(), Some((TokenKind::GreaterEqual, ">=")));
        assert_eq!(tokens.next(), Some((TokenKind::BangEqual, "!=")));
        assert_eq!(tokens.next(), Some((TokenKind::Less, "<")));
        assert_eq!(tokens.next(), Some((TokenKind::Greater, ">")));
        assert_eq!(tokens.next(), Some((TokenKind::Slash, "/")));
        assert_eq!(tokens.next(), Some((TokenKind::Dot, ".")));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn strings() {
        let mut tokens =
            tokenize_with_text(r#""" "stringěščřžýáíé" "this string has no close quote"#);

        assert_eq!(
            tokens.next(),
            Some((TokenKind::String { terminated: true }, r#""""#))
        );

        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));

        assert_eq!(
            tokens.next(),
            Some((
                TokenKind::String { terminated: true },
                r#""stringěščřžýáíé""#
            ))
        );

        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, " ")));

        assert_eq!(
            tokens.next(),
            Some((
                TokenKind::String { terminated: false },
                r#""this string has no close quote"#
            ))
        );

        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn unknown() {
        let mut tokens = tokenize_with_text("knowněščřžýáíéknown$");

        assert_eq!(tokens.next(), Some((TokenKind::Identifier, "known")));
        assert_eq!(tokens.next(), Some((TokenKind::Unknown, "ěščřžýáíé")));
        assert_eq!(tokens.next(), Some((TokenKind::Identifier, "known")));
        assert_eq!(tokens.next(), Some((TokenKind::Unknown, "$")));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn whitespace() {
        let mut tokens = tokenize_with_text("space    tabs\t\t\t\tnewlines\n\n\n\nend");

        assert_eq!(tokens.next(), Some((TokenKind::Identifier, "space")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, "    ")));
        assert_eq!(tokens.next(), Some((TokenKind::Identifier, "tabs")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, "\t\t\t\t")));
        assert_eq!(tokens.next(), Some((TokenKind::Identifier, "newlines")));
        assert_eq!(tokens.next(), Some((TokenKind::Whitespace, "\n\n\n\n")));
        assert_eq!(tokens.next(), Some((TokenKind::Identifier, "end")));
        assert_eq!(tokens.next(), None);
    }
}
