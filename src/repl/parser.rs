use crate::repl::lexer::{Lexer, Token};
use std::{collections::HashMap, io::Error, io::ErrorKind};

struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<Error>,
    prefix_parsers: HashMap<Token, PrefixParseFn>,
    infix_parsers: HashMap<Token, InfixParseFn>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer) -> Parser {
        let mut p = Parser {
            lexer: lexer,
            cur_token: Token::Illegal,
            peek_token: Token::Illegal,
            errors: Vec::new(),
            prefix_parsers: HashMap::new(),
            infix_parsers: HashMap::new(),
        };

        p.next_token();
        p.next_token();

        p
    }

    fn register_prefix(&mut self, token: Token, f: PrefixParseFn) {
        self.prefix_parsers.insert(token, f);
    }

    fn register_infix(&mut self, token: Token, f: InfixParseFn) {
        self.infix_parsers.insert(token, f);
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    // Match expectation by variant, not value. If matched, advance by a single
    // token.
    fn expect_peek(&mut self, token: Token) -> Result<(), Error> {
        let e = Err(Error::new(
            ErrorKind::Other,
            format!(
                "expected next token to be {:?}, got {:?}",
                token, self.peek_token
            ),
        ));

        match token {
            Token::Ident(_) => match self.peek_token {
                Token::Ident(_) => {
                    self.next_token();
                    return Ok(());
                }
                _ => {
                    return e;
                }
            },
            Token::Int(_) => match self.peek_token {
                Token::Int(_) => {
                    self.next_token();
                    return Ok(());
                }
                _ => {
                    return e;
                }
            },
            _ => {
                if token == self.peek_token {
                    self.next_token();
                    return Ok(());
                } else {
                    return e;
                }
            }
        }
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let token = self.cur_token.clone();
        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Some(Box::new(ExpressionStatement {
            token: token,
            value: None,
        }))
    }

    fn parse_expression(&self, p: Precedence) -> Option<Box<dyn Expression>> {
        if let Some(prefix) = self.prefix_parsers.get(&self.cur_token) {
            // return Some(Box::new(prefix()));
            return None;
        } else {
            None
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        let token = self.cur_token.clone();
        match self.expect_peek(Token::Ident(String::from("any"))) {
            Err(e) => {
                self.errors.push(e);
                return None;
            }
            _ => {}
        }

        let name = self.cur_token.clone();
        match self.expect_peek(Token::Assign) {
            Err(e) => {
                self.errors.push(e);
                return None;
            }
            _ => {}
        }

        // TODO: here we skip until hit semicolon
        while self.cur_token != Token::Semicolon {
            self.next_token();
        }

        Some(Box::new(LetStatement {
            token: token,
            name: name,
            value: None,
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let token = self.cur_token.clone();
        self.next_token();

        while self.cur_token != Token::Semicolon {
            self.next_token();
        }

        Some(Box::new(ReturnStatement {
            token: token,
            value: None,
        }))
    }

    fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Box<dyn Statement>> = Vec::new();
        let mut t = &self.cur_token;

        while *t != Token::EOF {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }

            self.next_token();
            t = &self.cur_token;
        }

        Program { statements }
    }
}

enum Precedence {
    Lowest = 1,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

trait Statement {
    fn get_token(&self) -> Token;
}

struct LetStatement {
    token: Token,
    value: Option<Box<dyn Expression>>,
    name: Token,
}

impl Statement for LetStatement {
    fn get_token(&self) -> Token {
        self.name.clone()
    }
}

struct ReturnStatement {
    token: Token,
    value: Option<Box<dyn Expression>>,
}

impl Statement for ReturnStatement {
    fn get_token(&self) -> Token {
        self.token.clone()
    }
}

// "one-liners that don't really do anything"
struct ExpressionStatement {
    token: Token,
    value: Option<Box<dyn Expression>>,
}

impl Statement for ExpressionStatement {
    fn get_token(&self) -> Token {
        self.token.clone()
    }
}

trait Expression {}

struct Program {
    statements: Vec<Box<dyn Statement>>,
}

type PrefixParseFn = fn() -> dyn Expression;

type InfixParseFn = fn(dyn Expression) -> dyn Expression;

#[test]
fn test_return_statements() {}

#[test]
fn test_let_statements() {
    let input = r#"let x = 5; let y = 10; let foobar = 8383;"#;
    let input = input.chars().collect();
    let l = Lexer::new(&input);
    let mut p = Parser::new(l);

    let prog = p.parse_program();
    assert_eq!(prog.statements.len(), 3);
    let expected = vec![
        Token::Ident(String::from("x")),
        Token::Ident(String::from("y")),
        Token::Ident(String::from("foobar")),
    ];

    assert_eq!(prog.statements[0].get_token(), expected[0]);
    assert_eq!(prog.statements[1].get_token(), expected[1]);
    assert_eq!(prog.statements[2].get_token(), expected[2]);
}
