#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Token {
    Illegal,
    EOF,

    Ident(String),
    Int(i32),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    LT,
    GT,

    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,

    Eq,
    NotEq,
}

// We only support basic ASCII here.
pub struct Lexer<'a> {
    input: &'a Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a Vec<char>) -> Self {
        let mut l = Lexer {
            input: input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };

        l.read_char();

        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position] as char;
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn eat_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.eat_whitespace();

        let token = match self.ch {
            '=' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    Token::Eq
                }
                _ => Token::Assign,
            },
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '-' => Token::Minus,
            '!' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    Token::NotEq
                }
                _ => Token::Bang,
            },
            '/' => Token::Slash,
            '*' => Token::Asterisk,
            '<' => Token::LT,
            '>' => Token::GT,
            '\0' => Token::EOF,
            _ => {
                if Lexer::is_letter(self.ch) {
                    return Lexer::token_from_word(self.read_identifier());
                } else if self.ch.is_digit(10) {
                    return Token::Int(self.read_number());
                } else {
                    Token::Illegal
                }
            }
        };

        self.read_char();

        token
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.position;

        while Lexer::is_letter(self.ch) {
            self.read_char();
        }

        self.input[pos..self.position].iter().collect()
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position]
        }
    }

    fn read_number(&mut self) -> i32 {
        let pos = self.position;

        while self.ch.is_digit(10) {
            self.read_char();
        }

        let d: String = self.input[pos..self.position].iter().collect();
        d.parse().unwrap()
    }

    fn is_letter(ch: char) -> bool {
        ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'
    }

    fn token_from_word(mut s: String) -> Token {
        s.make_ascii_lowercase();

        if s == "fn" {
            Token::Function
        } else if s == "let" {
            Token::Let
        } else if s == "true" {
            Token::True
        } else if s == "false" {
            Token::False
        } else if s == "if" {
            Token::If
        } else if s == "else" {
            Token::Else
        } else if s == "return" {
            Token::Return
        } else {
            Token::Ident(s.to_string())
        }
    }
}

#[test]
fn test_next_token() {
    let input = r#"let five = 5;
        let ten = 10;

        let add = fn(x, y) {
          x + y;
        };

        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;"#;

    let expected = vec![
        Token::Let,
        Token::Ident(String::from("five")),
        Token::Assign,
        Token::Int(5),
        Token::Semicolon,
        Token::Let,
        Token::Ident(String::from("ten")),
        Token::Assign,
        Token::Int(10),
        Token::Semicolon,
        Token::Let,
        Token::Ident(String::from("add")),
        Token::Assign,
        Token::Function,
        Token::LParen,
        Token::Ident(String::from("x")),
        Token::Comma,
        Token::Ident(String::from("y")),
        Token::RParen,
        Token::LBrace,
        Token::Ident(String::from("x")),
        Token::Plus,
        Token::Ident(String::from("y")),
        Token::Semicolon,
        Token::RBrace,
        Token::Semicolon,
        Token::Let,
        Token::Ident(String::from("result")),
        Token::Assign,
        Token::Ident(String::from("add")),
        Token::LParen,
        Token::Ident(String::from("five")),
        Token::Comma,
        Token::Ident(String::from("ten")),
        Token::RParen,
        Token::Semicolon,
        Token::Bang,
        Token::Minus,
        Token::Slash,
        Token::Asterisk,
        Token::Int(5),
        Token::Semicolon,
        Token::Int(5),
        Token::LT,
        Token::Int(10),
        Token::GT,
        Token::Int(5),
        Token::Semicolon,
        Token::If,
        Token::LParen,
        Token::Int(5),
        Token::LT,
        Token::Int(10),
        Token::RParen,
        Token::LBrace,
        Token::Return,
        Token::True,
        Token::Semicolon,
        Token::RBrace,
        Token::Else,
        Token::LBrace,
        Token::Return,
        Token::False,
        Token::Semicolon,
        Token::RBrace,
        Token::Int(10),
        Token::Eq,
        Token::Int(10),
        Token::Semicolon,
        Token::Int(10),
        Token::NotEq,
        Token::Int(9),
        Token::Semicolon,
        Token::EOF,
    ];

    let input = input.chars().collect();
    let mut lexer = Lexer::new(&input);
    for case in expected.iter() {
        let t = lexer.next_token();
        assert_eq!(t, *case);
    }
}
