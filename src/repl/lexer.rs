#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Token {
    pub t_type: TokenType,
    pub t_value: Option<TokenValue>,
}

impl Token {
    pub fn new() -> Token {
        Token {
            t_type: TokenType::Illegal,
            t_value: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum TokenValue {
    Numeric(i32),
    Literal(String),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum TokenType {
    Illegal,
    EOF,

    Ident,
    Int,

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

impl Token {
    pub fn string(&self) -> String {
        match self.t_type {
            TokenType::Illegal => return String::from(""),
            TokenType::EOF => String::from(""),

            TokenType::Ident => match self.t_value.as_ref().unwrap() {
                TokenValue::Literal(literal) => literal.to_string(),
                _ => String::from(""),
            },
            TokenType::Int => match self.t_value.as_ref().unwrap() {
                TokenValue::Numeric(num) => num.to_string(),
                _ => String::from(""),
            },
            TokenType::Assign => String::from("="),
            TokenType::Plus => String::from("+"),
            TokenType::Minus => String::from("-"),
            TokenType::Bang => String::from("!"),
            TokenType::Asterisk => String::from("*"),
            TokenType::Slash => String::from("/"),
            TokenType::LT => String::from("<"),
            TokenType::GT => String::from(">"),
            TokenType::Comma => String::from(","),
            TokenType::Semicolon => String::from(";"),
            TokenType::LParen => String::from("("),
            TokenType::RParen => String::from(")"),
            TokenType::LBrace => String::from("{"),
            TokenType::RBrace => String::from("}"),
            TokenType::Function => String::from("fn"),
            TokenType::Let => String::from("let"),
            TokenType::True => String::from("true"),
            TokenType::False => String::from("false"),
            TokenType::If => String::from("if"),
            TokenType::Else => String::from("else"),
            TokenType::Return => String::from("return"),
            TokenType::Eq => String::from("=="),
            TokenType::NotEq => String::from("!="),
        }
    }
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
                    Token {
                        t_type: TokenType::Eq,
                        t_value: None,
                    }
                }
                _ => Token {
                    t_type: TokenType::Assign,
                    t_value: None,
                },
            },
            ';' => Token {
                t_type: TokenType::Semicolon,
                t_value: None,
            },
            '(' => Token {
                t_type: TokenType::LParen,
                t_value: None,
            },
            ')' => Token {
                t_type: TokenType::RParen,
                t_value: None,
            },
            ',' => Token {
                t_type: TokenType::Comma,
                t_value: None,
            },
            '+' => Token {
                t_type: TokenType::Plus,
                t_value: None,
            },
            '{' => Token {
                t_type: TokenType::LBrace,
                t_value: None,
            },
            '}' => Token {
                t_type: TokenType::RBrace,
                t_value: None,
            },
            '-' => Token {
                t_type: TokenType::Minus,
                t_value: None,
            },
            '!' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    Token {
                        t_type: TokenType::NotEq,
                        t_value: None,
                    }
                }
                _ => Token {
                    t_type: TokenType::Bang,
                    t_value: None,
                },
            },
            '/' => Token {
                t_type: TokenType::Slash,
                t_value: None,
            },
            '*' => Token {
                t_type: TokenType::Asterisk,
                t_value: None,
            },
            '<' => Token {
                t_type: TokenType::LT,
                t_value: None,
            },
            '>' => Token {
                t_type: TokenType::GT,
                t_value: None,
            },
            '\0' => Token {
                t_type: TokenType::EOF,
                t_value: None,
            },
            _ => {
                if Lexer::is_letter(self.ch) {
                    return Lexer::token_from_word(self.read_identifier());
                } else if self.ch.is_digit(10) {
                    return Token {
                        t_type: TokenType::Int,
                        t_value: Some(TokenValue::Numeric(self.read_number())),
                    };
                } else {
                    Token {
                        t_type: TokenType::Illegal,
                        t_value: None,
                    }
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
            Token {
                t_type: TokenType::Function,
                t_value: None,
            }
        } else if s == "let" {
            Token {
                t_type: TokenType::Let,
                t_value: None,
            }
        } else if s == "true" {
            Token {
                t_type: TokenType::True,
                t_value: None,
            }
        } else if s == "false" {
            Token {
                t_type: TokenType::False,
                t_value: None,
            }
        } else if s == "if" {
            Token {
                t_type: TokenType::If,
                t_value: None,
            }
        } else if s == "else" {
            Token {
                t_type: TokenType::Else,
                t_value: None,
            }
        } else if s == "return" {
            Token {
                t_type: TokenType::Return,
                t_value: None,
            }
        } else {
            Token {
                t_type: TokenType::Ident,
                t_value: Some(TokenValue::Literal(s.to_string())),
            }
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
        Token {
            t_type: TokenType::Let,
            t_value: None,
        },
        Token {
            t_type: TokenType::Ident,
            t_value: Some(TokenValue::Literal(String::from("five"))),
        },
        Token {
            t_type: TokenType::Assign,
            t_value: None,
        },
        Token {
            t_type: TokenType::Int,
            t_value: Some(TokenValue::Numeric(5)),
        },
        Token {
            t_type: TokenType::Semicolon,
            t_value: None,
        },
        Token {
            t_type: TokenType::Let,
            t_value: None,
        },
        Token {
            t_type: TokenType::Ident,
            t_value: Some(TokenValue::Literal(String::from("ten"))),
        },
        Token {
            t_type: TokenType::Assign,
            t_value: None,
        },
        Token {
            t_type: TokenType::Int,
            t_value: Some(TokenValue::Numeric(10)),
        },
        Token {
            t_type: TokenType::Semicolon,
            t_value: None,
        },
        Token {
            t_type: TokenType::Let,
            t_value: None,
        },
        Token {
            t_type: TokenType::Ident,
            t_value: Some(TokenValue::Literal(String::from("add"))),
        },
        Token {
            t_type: TokenType::Assign,
            t_value: None,
        },
        Token {
            t_type: TokenType::Function,
            t_value: None,
        },
        Token {
            t_type: TokenType::LParen,
            t_value: None,
        },
        Token {
            t_type: TokenType::Ident,
            t_value: Some(TokenValue::Literal(String::from("x"))),
        },
        Token {
            t_type: TokenType::Comma,
            t_value: None,
        },
        Token {
            t_type: TokenType::Ident,
            t_value: Some(TokenValue::Literal(String::from("y"))),
        },
        Token {
            t_type: TokenType::RParen,
            t_value: None,
        },
        Token {
            t_type: TokenType::LBrace,
            t_value: None,
        },
        Token {
            t_type: TokenType::Ident,
            t_value: Some(TokenValue::Literal(String::from("x"))),
        },
        Token {
            t_type: TokenType::Plus,
            t_value: None,
        },
        Token {
            t_type: TokenType::Ident,
            t_value: Some(TokenValue::Literal(String::from("y"))),
        },
        Token {
            t_type: TokenType::Semicolon,
            t_value: None,
        },
        Token {
            t_type: TokenType::RBrace,
            t_value: None,
        },
        Token {
            t_type: TokenType::Semicolon,
            t_value: None,
        },
        Token {
            t_type: TokenType::Let,
            t_value: None,
        },
        Token {
            t_type: TokenType::Ident,
            t_value: Some(TokenValue::Literal(String::from("result"))),
        },
        Token {
            t_type: TokenType::Assign,
            t_value: None,
        },
        Token {
            t_type: TokenType::Ident,
            t_value: Some(TokenValue::Literal(String::from("add"))),
        },
        Token {
            t_type: TokenType::LParen,
            t_value: None,
        },
        Token {
            t_type: TokenType::Ident,
            t_value: Some(TokenValue::Literal(String::from("five"))),
        },
        Token {
            t_type: TokenType::Comma,
            t_value: None,
        },
        Token {
            t_type: TokenType::Ident,
            t_value: Some(TokenValue::Literal(String::from("ten"))),
        },
        Token {
            t_type: TokenType::RParen,
            t_value: None,
        },
        Token {
            t_type: TokenType::Semicolon,
            t_value: None,
        },
        Token {
            t_type: TokenType::Bang,
            t_value: None,
        },
        Token {
            t_type: TokenType::Minus,
            t_value: None,
        },
        Token {
            t_type: TokenType::Slash,
            t_value: None,
        },
        Token {
            t_type: TokenType::Asterisk,
            t_value: None,
        },
        Token {
            t_type: TokenType::Int,
            t_value: Some(TokenValue::Numeric(5)),
        },
        Token {
            t_type: TokenType::Semicolon,
            t_value: None,
        },
        Token {
            t_type: TokenType::Int,
            t_value: Some(TokenValue::Numeric(5)),
        },
        Token {
            t_type: TokenType::LT,
            t_value: None,
        },
        Token {
            t_type: TokenType::Int,
            t_value: Some(TokenValue::Numeric(10)),
        },
        Token {
            t_type: TokenType::GT,
            t_value: None,
        },
        Token {
            t_type: TokenType::Int,
            t_value: Some(TokenValue::Numeric(5)),
        },
        Token {
            t_type: TokenType::Semicolon,
            t_value: None,
        },
        Token {
            t_type: TokenType::If,
            t_value: None,
        },
        Token {
            t_type: TokenType::LParen,
            t_value: None,
        },
        Token {
            t_type: TokenType::Int,
            t_value: Some(TokenValue::Numeric(5)),
        },
        Token {
            t_type: TokenType::LT,
            t_value: None,
        },
        Token {
            t_type: TokenType::Int,
            t_value: Some(TokenValue::Numeric(10)),
        },
        Token {
            t_type: TokenType::RParen,
            t_value: None,
        },
        Token {
            t_type: TokenType::LBrace,
            t_value: None,
        },
        Token {
            t_type: TokenType::Return,
            t_value: None,
        },
        Token {
            t_type: TokenType::True,
            t_value: None,
        },
        Token {
            t_type: TokenType::Semicolon,
            t_value: None,
        },
        Token {
            t_type: TokenType::RBrace,
            t_value: None,
        },
        Token {
            t_type: TokenType::Else,
            t_value: None,
        },
        Token {
            t_type: TokenType::LBrace,
            t_value: None,
        },
        Token {
            t_type: TokenType::Return,
            t_value: None,
        },
        Token {
            t_type: TokenType::False,
            t_value: None,
        },
        Token {
            t_type: TokenType::Semicolon,
            t_value: None,
        },
        Token {
            t_type: TokenType::RBrace,
            t_value: None,
        },
        Token {
            t_type: TokenType::Int,
            t_value: Some(TokenValue::Numeric(10)),
        },
        Token {
            t_type: TokenType::Eq,
            t_value: None,
        },
        Token {
            t_type: TokenType::Int,
            t_value: Some(TokenValue::Numeric(10)),
        },
        Token {
            t_type: TokenType::Semicolon,
            t_value: None,
        },
        Token {
            t_type: TokenType::Int,
            t_value: Some(TokenValue::Numeric(10)),
        },
        Token {
            t_type: TokenType::NotEq,
            t_value: None,
        },
        Token {
            t_type: TokenType::Int,
            t_value: Some(TokenValue::Numeric(9)),
        },
        Token {
            t_type: TokenType::Semicolon,
            t_value: None,
        },
        Token {
            t_type: TokenType::EOF,
            t_value: None,
        },
    ];

    let input = input.chars().collect();
    let mut lexer = Lexer::new(&input);
    for case in expected.iter() {
        let t = lexer.next_token();
        assert_eq!(t, *case);
    }
}
