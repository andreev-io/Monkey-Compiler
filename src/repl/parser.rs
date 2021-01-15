use crate::repl::lexer::{Lexer, Token, TokenType, TokenValue};
use std::{io::Error, io::ErrorKind};

struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<Error>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer) -> Parser {
        let mut p = Parser {
            lexer: lexer,
            cur_token: Token::new(),
            peek_token: Token::new(),
            errors: Vec::new(),
        };

        p.next_token();
        p.next_token();

        p
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn prefix_parse(&mut self) -> Option<Box<dyn Expression>> {
        match self.cur_token.t_type {
            TokenType::Ident => Some(Box::new(Identifier {
                token: self.cur_token.clone(),
            })),
            TokenType::Int => Some(Box::new(IntegerLiteral {
                token: self.cur_token.clone(),
            })),
            TokenType::Bang | TokenType::Minus => {
                let cur_token = self.cur_token.clone();
                self.next_token();

                let right_expression = self.parse_expression(Precedence::Prefix);
                return Some(Box::new(PrefixExpression {
                    token: cur_token,
                    right: right_expression,
                }));
            }
            _ => None,
        }
    }

    fn get_precedence(token_type: TokenType) -> Precedence {
        match token_type {
            TokenType::Eq | TokenType::NotEq => Precedence::Equals,
            TokenType::LT | TokenType::GT => Precedence::LessGreater,
            TokenType::Plus | TokenType::Minus => Precedence::Sum,
            TokenType::Slash | TokenType::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }

    fn infix_parse(&mut self, left: Box<impl Expression + 'static>) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();

        let precedence = Parser::get_precedence(self.cur_token.t_type.clone());

        self.next_token();
        let right = self.parse_expression(precedence);

        Some(Box::new(InfixExpression {
            left: Some(left),
            right,
            token,
        }))
    }

    // Match expectation by variant, not value. If matched, advance by a single
    // token.
    fn expect_peek(&mut self, token_type: TokenType) -> Result<(), Error> {
        if token_type == self.peek_token.t_type {
            self.next_token();
            return Ok(());
        } else {
            return Err(Error::new(
                ErrorKind::Other,
                format!(
                    "expected next token to be {:?}, got {:?}",
                    token_type, self.peek_token.t_type
                ),
            ));
        }
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token.t_type {
            TokenType::Let => match self.parse_let_statement() {
                Some(statement) => Some(Box::new(statement)),
                None => None,
            },
            TokenType::Return => match self.parse_return_statement() {
                Some(statement) => Some(Box::new(statement)),
                None => None,
            },
            _ => match self.parse_expression_statement() {
                Some(statement) => Some(Box::new(statement)),
                None => None,
            },
        }
    }

    fn parse_let_statement(&mut self) -> Option<impl Statement> {
        let token = self.cur_token.clone();
        match self.expect_peek(TokenType::Ident) {
            Err(e) => {
                self.errors.push(e);
                return None;
            }
            _ => {}
        }

        let name = self.cur_token.clone();
        match self.expect_peek(TokenType::Assign) {
            Err(e) => {
                self.errors.push(e);
                return None;
            }
            _ => {}
        }

        // TODO: here we skip until hit semicolon
        while self.cur_token.t_type != TokenType::Semicolon {
            self.next_token();
        }

        // TODO: none value for now
        Some(LetStatement {
            token: token,
            name: name,
            value: None,
        })
    }

    fn parse_expression_statement(&mut self) -> Option<impl Statement> {
        let token = self.cur_token.clone();
        let expression = self.parse_expression(Precedence::Lowest);
        if self.peek_token.t_type == TokenType::Semicolon {
            self.next_token();
        }

        Some(ExpressionStatement {
            token: token,
            value: expression,
        })
    }

    fn parse_expression(&mut self, p: Precedence) -> Option<Box<dyn Expression>> {
        let mut left = self.prefix_parse();

        while self.peek_token.t_type != TokenType::Semicolon
            && p < Parser::get_precedence(self.peek_token.t_type.clone())
        {
            self.next_token();

            if let Some(inner_left) = left {
                let token = self.cur_token.clone();

                let precedence = Parser::get_precedence(self.cur_token.t_type.clone());

                self.next_token();
                let right = self.parse_expression(precedence);

                left = Some(Box::new(InfixExpression {
                    left: Some(inner_left),
                    right,
                    token,
                }))
            } else {
                return left;
            }
        }

        left
    }

    fn parse_return_statement(&mut self) -> Option<impl Statement> {
        let token = self.cur_token.clone();
        self.next_token();

        while self.cur_token.t_type != TokenType::Semicolon {
            self.next_token();
        }

        // TODO: None value for now
        Some(ReturnStatement {
            token: token,
            value: None,
        })
    }

    fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Box<dyn Statement>> = Vec::new();
        let mut t = &self.cur_token;
        while t.t_type != TokenType::EOF {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }

            self.next_token();
            t = &self.cur_token;
        }

        Program { statements }
    }
}

#[derive(PartialEq, Eq, PartialOrd)]
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
    fn get_expression(&self) -> &Option<Box<dyn Expression>>;
    fn string(&self) -> String;
}

struct LetStatement {
    // token is the let token
    token: Token,

    // value is the RHS expression
    value: Option<Box<dyn Expression>>,

    // name is the LHS identiier
    name: Token,
}

impl Statement for LetStatement {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_expression(&self) -> &Option<Box<dyn Expression>> {
        &self.value
    }

    fn string(&self) -> String {
        let mut s: String = String::from("");
        s.push_str(&self.token.string());
        s.push_str(" ");
        s.push_str(&self.name.string());
        s.push_str(" = ");

        if let Some(exp) = &self.value {
            s.push_str(&exp.string());
        }

        s.push_str(";");

        s
    }
}

struct ReturnStatement {
    // token is the return token
    token: Token,
    // value is the RHS expression
    value: Option<Box<dyn Expression>>,
}

impl Statement for ReturnStatement {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_expression(&self) -> &Option<Box<dyn Expression>> {
        &self.value
    }

    fn string(&self) -> String {
        let mut s: String = String::from("");

        s.push_str(&self.token.string());
        s.push_str(" ");

        if let Some(exp) = &self.value {
            s.push_str(&exp.string());
        }

        s.push_str(";");

        s
    }
}

// One-liners that don't do much, say x+5;
struct ExpressionStatement {
    // first token of the expression
    token: Token,
    // value of the expression
    value: Option<Box<dyn Expression>>,
}

impl Statement for ExpressionStatement {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_expression(&self) -> &Option<Box<dyn Expression>> {
        &self.value
    }

    fn string(&self) -> String {
        let mut s: String = String::from("");

        if let Some(exp) = &self.value {
            s.push_str(&exp.string());
        }

        s
    }
}

trait Expression {
    fn get_token(&self) -> Token;
    fn get_left_subexpression(&self) -> &Option<Box<dyn Expression>>;
    fn get_right_subexpression(&self) -> &Option<Box<dyn Expression>>;
    fn string(&self) -> String;
}

struct InfixExpression {
    // in 5+5, token is +
    token: Token,
    // LHS expression
    left: Option<Box<dyn Expression>>,
    // RHS expression
    right: Option<Box<dyn Expression>>,
}

impl Expression for InfixExpression {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_left_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &self.left
    }

    fn get_right_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &self.right
    }

    fn string(&self) -> String {
        let mut s: String = String::from("");

        s.push_str("(");
        if let Some(exp) = &self.left {
            s.push_str(&exp.string());
        }

        s.push_str(&format!(" {} ", self.token.string()));

        if let Some(exp) = &self.right {
            s.push_str(&exp.string());
        }
        s.push_str(")");
        s
    }
}

struct PrefixExpression {
    // in !true, token is !
    token: Token,
    // RHS expression
    right: Option<Box<dyn Expression>>,
}

impl Expression for PrefixExpression {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_left_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn get_right_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &self.right
    }

    fn string(&self) -> String {
        let mut s: String = String::from("");

        s.push_str("(");
        s.push_str(&self.token.string());
        if let Some(exp) = &self.right {
            s.push_str(&exp.string());
        }

        s.push_str(")");

        s
    }
}

// simplest expression
struct Identifier {
    token: Token,
}

impl Expression for Identifier {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_left_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn get_right_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn string(&self) -> String {
        self.token.string()
    }
}

struct IntegerLiteral {
    token: Token,
}

impl Expression for IntegerLiteral {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_left_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn get_right_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn string(&self) -> String {
        self.token.string()
    }
}

struct Program {
    statements: Vec<Box<dyn Statement>>,
}

impl Program {
    fn string(&self) -> String {
        let mut s: String = String::from("");
        for statement in &self.statements {
            s.push_str(&statement.string());
        }

        s
    }
}

#[test]
fn test_return_statements() {
    let input = r#"return 5;
    return 10;
    return 838383;"#
        .chars()
        .collect();

    let l = Lexer::new(&input);
    let mut p = Parser::new(l);
    let program = p.parse_program();

    assert_eq!(program.statements.len(), 3);
    assert_eq!(
        program.statements[0].get_token(),
        Token {
            t_value: None,
            t_type: TokenType::Return
        }
    );
    assert_eq!(
        program.statements[1].get_token(),
        Token {
            t_value: None,
            t_type: TokenType::Return
        }
    );
    assert_eq!(
        program.statements[2].get_token(),
        Token {
            t_value: None,
            t_type: TokenType::Return
        }
    );

    assert_eq!("return ;", program.statements[0].string());
    assert_eq!("return ;", program.statements[1].string());
    assert_eq!("return ;", program.statements[2].string());
}

#[test]
fn test_let_statements() {
    let input = r#"let x = 5;
    let y = 10;
    let foobar = 838383;"#
        .chars()
        .collect();

    let l = Lexer::new(&input);
    let mut p = Parser::new(l);
    let program = p.parse_program();

    assert_eq!(program.statements.len(), 3);
    assert_eq!(
        program.statements[0].get_token(),
        Token {
            t_value: None,
            t_type: TokenType::Let
        }
    );
    assert_eq!(
        program.statements[1].get_token(),
        Token {
            t_value: None,
            t_type: TokenType::Let
        }
    );
    assert_eq!(
        program.statements[2].get_token(),
        Token {
            t_value: None,
            t_type: TokenType::Let
        }
    );

    assert_eq!("let x = ;", program.statements[0].string());
    assert_eq!("let y = ;", program.statements[1].string());
    assert_eq!("let foobar = ;", program.statements[2].string());
}

#[test]
fn test_identifier_expression() {
    let input = String::from("foobar;");
    let c = input.chars().collect();

    let l = Lexer::new(&c);
    let mut p = Parser::new(l);
    let program = p.parse_program();

    assert_eq!(program.statements.len(), 1);
    let token = program.statements[0].get_token();

    if let Some(val) = token.t_value {
        assert!(token.t_type == TokenType::Ident);
        assert!(val == TokenValue::Literal(String::from("foobar")));
    } else {
        panic!("empty token")
    }
}

#[test]
fn test_integer_literal_expression() {
    let input = String::from("5;");
    let c = input.chars().collect();

    let l = Lexer::new(&c);
    let mut p = Parser::new(l);
    let program = p.parse_program();

    assert_eq!(program.statements.len(), 1);
    let token = program.statements[0].get_token();

    if let Some(val) = token.t_value {
        assert!(token.t_type == TokenType::Int);
        assert!(val == TokenValue::Numeric(5));
    } else {
        panic!("empty token")
    }
}

#[test]
fn test_prefix_expression() {
    // !5; is an expression statement and also a prefix expression
    let input = String::from("!5;");
    let c = input.chars().collect();

    let l = Lexer::new(&c);
    let mut p = Parser::new(l);
    let program = p.parse_program();

    assert_eq!(program.statements.len(), 1);
    let token = program.statements[0].get_token();
    assert_eq!(token.t_type, TokenType::Bang);
    let inner_expression = program.statements[0].get_expression();
    if let Some(exp) = inner_expression {
        let right = exp.get_right_subexpression();
        if let Some(right) = right {
            assert_eq!(right.get_token().t_type, TokenType::Int);
            assert_eq!(right.get_token().t_value, Some(TokenValue::Numeric(5)));
        } else {
            panic!("no right subexpression");
        }
    } else {
        panic!("no inner expression");
    }

    let input = String::from("-15;");
    let c = input.chars().collect();

    let l = Lexer::new(&c);
    let mut p = Parser::new(l);
    let program = p.parse_program();

    assert_eq!(program.statements.len(), 1);
    let token = program.statements[0].get_token();
    assert_eq!(token.t_type, TokenType::Minus);
    let inner_expression = program.statements[0].get_expression();
    if let Some(exp) = inner_expression {
        let right = exp.get_right_subexpression();
        if let Some(right) = right {
            assert_eq!(right.get_token().t_type, TokenType::Int);
            assert_eq!(right.get_token().t_value, Some(TokenValue::Numeric(15)));
        } else {
            panic!("no right subexpression");
        }
    } else {
        panic!("no inner expression");
    }
}

#[test]
fn test_infix_expression() {
    struct InfixTest {
        input: String,
        left_int: i32,
        right_int: i32,
        operator: Token,
    }

    let v = vec![
        InfixTest {
            input: String::from("5+5;"),
            left_int: 5,
            right_int: 5,
            operator: Token {
                t_type: TokenType::Plus,
                t_value: None,
            },
        },
        InfixTest {
            input: String::from(" 5 < 6"),
            left_int: 5,
            right_int: 6,
            operator: Token {
                t_type: TokenType::LT,
                t_value: None,
            },
        },
        InfixTest {
            input: String::from("5 != 7  ;"),
            left_int: 5,
            right_int: 7,
            operator: Token {
                t_type: TokenType::NotEq,
                t_value: None,
            },
        },
        InfixTest {
            input: String::from("5 / 1"),
            left_int: 5,
            right_int: 1,
            operator: Token {
                t_type: TokenType::Slash,
                t_value: None,
            },
        },
    ];

    for t in v.iter() {
        let input = t.input.chars().collect();
        let l = Lexer::new(&input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(program.statements.len(), 1);
        let exp = program.statements[0].get_expression();
        if let Some(exp) = exp {
            match (exp.get_left_subexpression(), exp.get_right_subexpression()) {
                (Some(e_left), Some(e_right)) => {
                    assert_eq!(
                        e_left.get_token().t_value,
                        Some(TokenValue::Numeric(t.left_int))
                    );
                    assert_eq!(
                        e_right.get_token().t_value,
                        Some(TokenValue::Numeric(t.right_int))
                    );
                }
                _ => panic!("inner expression missing"),
            }
            assert_eq!(exp.get_token(), t.operator);
        } else {
            panic!("no inner expression");
        }
    }
}

#[test]
fn test_operator_precedence() {
    struct PrecedenceTest {
        input: Vec<char>,
        output: String,
    }

    let v = vec![
        PrecedenceTest {
            input: "-a * b".chars().collect(),
            output: String::from("((-a) * b)"),
        },
        PrecedenceTest {
            input: "a + b * c + d / e - f".chars().collect(),
            output: String::from("(((a + (b * c)) + (d / e)) - f)"),
        },
        PrecedenceTest {
            input: "3 + 4 * 5 == 3 * 1 + 4 * 5".chars().collect(),
            output: String::from("((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
        },
    ];

    for test in v.iter() {
        let l = Lexer::new(&test.input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(test.output, program.string());
    }
}
