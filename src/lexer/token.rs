pub mod token {
    #[derive(Debug, PartialEq)]
    pub enum Token {
        ILLEGAL,
        EOF,

        IDENT(String),
        INT(i32),

        ASSIGN,
        PLUS,
        MINUS,
        BANG,
        ASTERISK,
        SLASH,

        LT,
        GT,

        COMMA,
        SEMICOLON,

        LPAREN,
        RPAREN,
        LBRACE,
        RBRACE,

        FUNCTION,
        LET,
        TRUE,
        FALSE,
        IF,
        ELSE,
        RETURN,

        EQ,
        NOTEQ,
    }

    // We only support basic ASCII here.
    pub struct Lexer {
        input: Vec<char>,
        position: usize,
        read_position: usize,
        ch: char,
    }

    impl Lexer {
        pub fn new(input: String) -> Lexer {
            let mut l = Lexer {
                input: input.chars().collect(),
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
                        Token::EQ
                    }
                    _ => Token::ASSIGN,
                },
                ';' => Token::SEMICOLON,
                '(' => Token::LPAREN,
                ')' => Token::RPAREN,
                ',' => Token::COMMA,
                '+' => Token::PLUS,
                '{' => Token::LBRACE,
                '}' => Token::RBRACE,
                '-' => Token::MINUS,
                '!' => match self.peek_char() {
                    '=' => {
                        self.read_char();
                        Token::NOTEQ
                    }
                    _ => Token::BANG,
                },
                '/' => Token::SLASH,
                '*' => Token::ASTERISK,
                '<' => Token::LT,
                '>' => Token::GT,
                '\0' => Token::EOF,
                _ => {
                    if is_letter(self.ch) {
                        return token_from_word(self.read_identifier());
                    } else if self.ch.is_digit(10) {
                        return Token::INT(self.read_number());
                    } else {
                        Token::ILLEGAL
                    }
                }
            };

            self.read_char();

            token
        }

        fn read_identifier(&mut self) -> String {
            let pos = self.position;

            while is_letter(self.ch) {
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
    }

    fn is_letter(ch: char) -> bool {
        ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'
    }

    fn token_from_word(mut s: String) -> Token {
        s.make_ascii_lowercase();

        if s == "fn" {
            Token::FUNCTION
        } else if s == "let" {
            Token::LET
        } else if s == "true" {
            Token::TRUE
        } else if s == "false" {
            Token::FALSE
        } else if s == "if" {
            Token::IF
        } else if s == "else" {
            Token::ELSE
        } else if s == "return" {
            Token::RETURN
        } else {
            Token::IDENT(s)
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
            Token::LET,
            Token::IDENT(String::from("five")),
            Token::ASSIGN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(String::from("ten")),
            Token::ASSIGN,
            Token::INT(10),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(String::from("add")),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT(String::from("x")),
            Token::COMMA,
            Token::IDENT(String::from("y")),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT(String::from("x")),
            Token::PLUS,
            Token::IDENT(String::from("y")),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(String::from("result")),
            Token::ASSIGN,
            Token::IDENT(String::from("add")),
            Token::LPAREN,
            Token::IDENT(String::from("five")),
            Token::COMMA,
            Token::IDENT(String::from("ten")),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::INT(5),
            Token::SEMICOLON,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::GT,
            Token::INT(5),
            Token::SEMICOLON,
            Token::IF,
            Token::LPAREN,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::RPAREN,
            Token::LBRACE,
            Token::RETURN,
            Token::TRUE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::ELSE,
            Token::LBRACE,
            Token::RETURN,
            Token::FALSE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::INT(10),
            Token::EQ,
            Token::INT(10),
            Token::SEMICOLON,
            Token::INT(10),
            Token::NOTEQ,
            Token::INT(9),
            Token::SEMICOLON,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input.to_string());
        for case in expected.iter() {
            let t = lexer.next_token();
            assert_eq!(t, *case);
        }
    }
}
