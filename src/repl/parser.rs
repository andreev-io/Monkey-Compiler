use crate::repl::lexer::{Lexer, Token, TokenType};
use std::{io::Error, io::ErrorKind};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<Error>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer) -> Parser {
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

    fn parse_prefix(&mut self) -> Box<Expression> {
        let token = self.cur_token.clone();

        match self.cur_token.t_type {
            TokenType::String => Box::new(Expression::String(token)),
            TokenType::LBracket => {
                let elements = self.parse_arguments(TokenType::RBracket);
                Box::new(Expression::Array(elements))
            }
            TokenType::Ident => Box::new(Expression::Identifier(token)),
            TokenType::Int => Box::new(Expression::Integer(token)),
            TokenType::Bang | TokenType::Minus => {
                let cur_token = self.cur_token.clone();
                self.next_token();

                let right_expression = self.parse_expression(Precedence::Prefix);
                Box::new(Expression::Prefix(token, right_expression))
            }
            TokenType::False | TokenType::True => Box::new(Expression::Boolean(token)),
            TokenType::LParen => {
                self.next_token();
                let expression = self.parse_expression(Precedence::Lowest);
                match self.expect_peek(TokenType::RParen) {
                    Ok(()) => expression,
                    _ => Box::new(Expression::None),
                }
            }
            TokenType::If => {
                let token = self.cur_token.clone();

                if self.expect_peek(TokenType::LParen).is_err() {
                    return Box::new(Expression::None);
                }

                self.next_token();
                let condition = self.parse_expression(Precedence::Lowest);

                if self.expect_peek(TokenType::RParen).is_err() {
                    return Box::new(Expression::None);
                }

                if self.expect_peek(TokenType::LBrace).is_err() {
                    return Box::new(Expression::None);
                }

                let consequence = self.parse_block_statement();

                let alternative = if self.peek_token.t_type == TokenType::Else {
                    self.next_token();

                    if self.expect_peek(TokenType::LBrace).is_err() {
                        return Box::new(Expression::None);
                    }

                    self.parse_block_statement()
                } else {
                    Box::new(Statement::None)
                };

                Box::new(Expression::If(condition, consequence, alternative))
            }
            TokenType::Function => {
                let token = self.cur_token.clone();
                if self.expect_peek(TokenType::LParen).is_err() {
                    return Box::new(Expression::None);
                }

                let parameters = self.parse_function_params();
                if self.expect_peek(TokenType::LBrace).is_err() {
                    return Box::new(Expression::None);
                }

                let body = self.parse_block_statement();
                Box::new(Expression::Function(parameters, body))
            }
            _ => Box::new(Expression::None),
        }
    }

    fn parse_function_params(&mut self) -> Vec<Token> {
        let mut v = Vec::new();

        if self.peek_token.t_type == TokenType::RParen {
            self.next_token();
            return v;
        }

        self.next_token();
        match self.cur_token.t_type {
            TokenType::Ident => {}
            _ => return Vec::new(),
        }

        v.push(self.cur_token.clone());

        while self.peek_token.t_type == TokenType::Comma {
            self.next_token();
            self.next_token();

            match self.cur_token.t_type {
                TokenType::Ident => {}
                _ => return Vec::new(),
            }

            v.push(self.cur_token.clone());
        }

        if self.expect_peek(TokenType::RParen).is_err() {
            return Vec::new();
        }

        v
    }

    fn parse_block_statement(&mut self) -> Box<Statement> {
        let token = self.cur_token.clone();
        self.next_token();
        let mut statements = Vec::new();

        while self.cur_token.t_type != TokenType::RBrace && self.cur_token.t_type != TokenType::EOF
        {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }
            self.next_token();
        }

        Box::new(Statement::Block(statements))
    }

    fn get_precedence(token_type: TokenType) -> Precedence {
        match token_type {
            TokenType::Eq | TokenType::NotEq => Precedence::Equals,
            TokenType::LT | TokenType::GT => Precedence::LessGreater,
            TokenType::Plus | TokenType::Minus => Precedence::Sum,
            TokenType::Slash | TokenType::Asterisk => Precedence::Product,
            TokenType::LParen => Precedence::Call,
            TokenType::LBracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }

    fn parse_infix(&mut self, left: Box<Expression>) -> Box<Expression> {
        let token = self.cur_token.clone();
        match token.t_type {
            TokenType::LBracket => {
                self.next_token();
                let right = self.parse_expression(Precedence::Lowest);
                if self.expect_peek(TokenType::RBracket).is_err() {
                    Box::new(Expression::None)
                } else {
                    Box::new(Expression::Index(left, right))
                }
            }
            TokenType::LParen => {
                let arguments = self.parse_arguments(TokenType::RParen);
                Box::new(Expression::Call(left, arguments))
            }
            _ => {
                let precedence = Parser::get_precedence(self.cur_token.t_type.clone());

                self.next_token();
                let right = self.parse_expression(precedence);

                Box::new(Expression::Infix(left, token, right))
            }
        }
    }

    fn parse_arguments(&mut self, typage: TokenType) -> Vec<Box<Expression>> {
        let mut v = Vec::new();
        if self.peek_token.t_type == typage {
            self.next_token();
            return v;
        }

        self.next_token();
        v.push(self.parse_expression(Precedence::Lowest));

        while self.peek_token.t_type == TokenType::Comma {
            self.next_token();
            self.next_token();

            v.push(self.parse_expression(Precedence::Lowest));
        }

        if self.expect_peek(typage).is_err() {
            return Vec::new();
        }

        v
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

    fn parse_statement(&mut self) -> Option<Box<Statement>> {
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

    fn parse_let_statement(&mut self) -> Option<Statement> {
        match self.expect_peek(TokenType::Ident) {
            Err(e) => {
                self.errors.push(e);
                return None;
            }
            _ => {}
        }

        let token = self.cur_token.clone();

        match self.expect_peek(TokenType::Assign) {
            Err(e) => {
                self.errors.push(e);
                return None;
            }
            _ => {}
        }

        self.next_token();
        let value = self.parse_expression(Precedence::Lowest);
        if self.peek_token.t_type == TokenType::Semicolon {
            self.next_token();
        }

        Some(Statement::Let(token, value))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        let expression = self.parse_expression(Precedence::Lowest);
        if self.peek_token.t_type == TokenType::Semicolon {
            self.next_token();
        }

        Some(Statement::Expression(expression))
    }

    // This and the callee methods are the core idea of Pratt parsing. The
    // ultimate goal is to have higher precedence operations deeper in the tree
    // than the lower precedence operations.
    //
    // Two parameters matter: the precedence argument represents the
    // right-binding power, the power of the expression to bind the expressions
    // to the right of it to itself. The left-binding power comes from peeked
    // precedence. Consider 2 + 3 * 4. 3 here is an expression. The left-binding
    // power of * is higher than the right-binding power of +, so * consumes 3
    // as its left child node. Hence the name top-down operator precedence.
    //
    // The rest of the idea is that the same token can be treated differently in
    // prefix & infix contexts. Consider, say 2 - -3 * 4. Here -3 is parsed as
    // the left child node. Notice how prefixes are defined to have high
    // precedence.
    fn parse_expression(&mut self, p: Precedence) -> Box<Expression> {
        let mut left = self.parse_prefix();

        while self.peek_token.t_type != TokenType::Semicolon
            && p < Parser::get_precedence(self.peek_token.t_type.clone())
        {
            self.next_token();

            left = self.parse_infix(left);
        }

        left
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest);
        if self.peek_token.t_type == TokenType::Semicolon {
            self.next_token();
        }

        Some(Statement::Return(value))
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Box<Statement>> = Vec::new();
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

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd)]
enum Precedence {
    Lowest = 1,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expression {
    // left array expression, right index expression
    Index(Box<Expression>, Box<Expression>),
    // left function expression, right vector of arguments
    Call(Box<Expression>, Vec<Box<Expression>>),
    Array(Vec<Box<Expression>>),
    Infix(Box<Expression>, Token, Box<Expression>),
    Prefix(Token, Box<Expression>),
    Boolean(Token),
    Identifier(Token),
    String(Token),
    Integer(Token),
    // condition, block statement, else clause
    If(Box<Expression>, Box<Statement>, Box<Statement>),
    // vector of identifier tokens and the body
    Function(Vec<Token>, Box<Statement>),
    None,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Statement {
    Block(Vec<Box<Statement>>),
    Let(Token, Box<Expression>),
    Expression(Box<Expression>),
    Return(Box<Expression>),
    None,
}

pub struct Program {
    pub statements: Vec<Box<Statement>>,
}

// #[cfg(test)]
// mod test {
//     use crate::repl::{
//         eval::Node,
//         lexer::{Lexer, Token, TokenType, TokenValue},
//         object::Environment,
//         parser::Parser,
//     };

//     #[test]
//     fn test_function_literal() {
//         let input = r#"fn(x, y) {   x+ y }"#.chars().collect();
//         let lexer = Lexer::new(&input);
//         let mut parser = Parser::new(lexer);
//         let program = parser.parse_program();

//         let statements = program.statements;
//         assert_eq!(statements.len(), 1);
//         if let Some(exp) = statements[0].get_expression() {
//             assert_eq!(TokenType::Function, exp.get_token().t_type);
//             assert_eq!("fn(x, y) { (x + y) }", exp.string());
//         } else {
//             panic!("missing inner expression");
//         }

//         let input = r#"fn() {   x+ y }"#.chars().collect();
//         let lexer = Lexer::new(&input);
//         let mut parser = Parser::new(lexer);
//         let program = parser.parse_program();

//         let statements = program.statements;
//         assert_eq!(statements.len(), 1);
//         if let Some(exp) = statements[0].get_expression() {
//             assert_eq!(TokenType::Function, exp.get_token().t_type);
//             assert_eq!("fn() { (x + y) }", exp.string());
//         } else {
//             panic!("missing inner expression");
//         }
//     }

//     #[test]
//     fn test_call_expression() {
//         let input = "add(1, 2*3, 4+  5)".chars().collect();
//         let l = Lexer::new(&input);
//         let mut parser = Parser::new(l);
//         let program = parser.parse_program();

//         assert_eq!("add(1, (2 * 3), (4 + 5));", program.string());
//         assert_eq!(program.statements.len(), 1);
//         if let Some(exp) = program.statements[0].get_expression() {
//             assert_eq!("add(1, (2 * 3), (4 + 5));", exp.string());
//         } else {
//             panic!("missing inner expression");
//         }
//     }

//     #[test]
//     fn test_return_statements() {
//         let input = r#"return 5;
//     return 10;
//     return 838383"#
//             .chars()
//             .collect();

//         let l = Lexer::new(&input);
//         let mut p = Parser::new(l);
//         let program = p.parse_program();

//         assert_eq!(program.statements.len(), 3);
//         assert_eq!(
//             program.statements[0].get_token(),
//             Token {
//                 t_value: None,
//                 t_type: TokenType::Return
//             }
//         );
//         assert_eq!(
//             program.statements[1].get_token(),
//             Token {
//                 t_value: None,
//                 t_type: TokenType::Return
//             }
//         );
//         assert_eq!(
//             program.statements[2].get_token(),
//             Token {
//                 t_value: None,
//                 t_type: TokenType::Return
//             }
//         );

//         assert_eq!("return 5;", program.statements[0].string());
//         assert_eq!("return 10;", program.statements[1].string());
//         assert_eq!("return 838383;", program.statements[2].string());
//     }

//     #[test]
//     fn test_let_statements() {
//         let input = r#"let x = 5;
//     let y = 10;
//     let foobar = 838383"#
//             .chars()
//             .collect();

//         let l = Lexer::new(&input);
//         let mut p = Parser::new(l);
//         let program = p.parse_program();

//         assert_eq!(program.statements.len(), 3);
//         assert_eq!(
//             program.statements[0].get_token(),
//             Token {
//                 t_value: None,
//                 t_type: TokenType::Let
//             }
//         );
//         assert_eq!(
//             program.statements[1].get_token(),
//             Token {
//                 t_value: None,
//                 t_type: TokenType::Let
//             }
//         );
//         assert_eq!(
//             program.statements[2].get_token(),
//             Token {
//                 t_value: None,
//                 t_type: TokenType::Let
//             }
//         );

//         assert_eq!("let x = 5;", program.statements[0].string());
//         assert_eq!("let y = 10;", program.statements[1].string());
//         assert_eq!("let foobar = 838383;", program.statements[2].string());
//     }

//     #[test]
//     fn test_identifier_expression() {
//         let input = String::from("foobar;");
//         let c = input.chars().collect();

//         let l = Lexer::new(&c);
//         let mut p = Parser::new(l);
//         let program = p.parse_program();

//         assert_eq!(program.statements.len(), 1);
//         let token = program.statements[0].get_token();

//         if let Some(val) = token.t_value {
//             assert!(token.t_type == TokenType::Ident);
//             assert!(val == TokenValue::Literal(String::from("foobar")));
//         } else {
//             panic!("empty token")
//         }
//     }

//     #[test]
//     fn test_boolean_literal_expression() {
//         let input = String::from("false;");
//         let c = input.chars().collect();

//         let l = Lexer::new(&c);
//         let mut p = Parser::new(l);
//         let program = p.parse_program();

//         assert_eq!(program.statements.len(), 1);
//         let token = program.statements[0].get_token();

//         assert!(token.t_type == TokenType::False);
//         assert_eq!(
//             program.statements[0]
//                 .get_expression()
//                 .as_ref()
//                 .unwrap()
//                 .string(),
//             String::from("false")
//         );

//         let input = String::from("false == false");
//         let c = input.chars().collect();

//         let l = Lexer::new(&c);
//         let mut p = Parser::new(l);
//         let program = p.parse_program();

//         assert_eq!(program.statements.len(), 1);
//         if let Some(exp) = program.statements[0].get_expression() {
//             assert_eq!(TokenType::Eq, exp.get_token().t_type);
//             assert_eq!("(false == false)", exp.string());
//             assert_eq!(
//                 "false",
//                 exp.get_left_subexpression().as_ref().unwrap().string()
//             );
//             assert_eq!(
//                 "false",
//                 exp.get_right_subexpression().as_ref().unwrap().string()
//             );
//         } else {
//             panic!("no inner expression");
//         }
//     }

//     #[test]
//     fn test_integer_literal_expression() {
//         let input = String::from("5;");
//         let c = input.chars().collect();

//         let l = Lexer::new(&c);
//         let mut p = Parser::new(l);
//         let program = p.parse_program();

//         assert_eq!(program.statements.len(), 1);
//         let token = program.statements[0].get_token();

//         if let Some(val) = token.t_value {
//             assert!(token.t_type == TokenType::Int);
//             assert!(val == TokenValue::Numeric(5));
//         } else {
//             panic!("empty token")
//         }
//     }

//     #[test]
//     fn test_prefix_expression() {
//         // !5; is an expression statement and also a prefix expression
//         let input = String::from("!5;");
//         let c = input.chars().collect();

//         let l = Lexer::new(&c);
//         let mut p = Parser::new(l);
//         let program = p.parse_program();

//         assert_eq!(program.statements.len(), 1);
//         let token = program.statements[0].get_token();
//         assert_eq!(token.t_type, TokenType::Bang);
//         let inner_expression = program.statements[0].get_expression();
//         if let Some(exp) = inner_expression {
//             let right = exp.get_right_subexpression();
//             if let Some(right) = right {
//                 assert_eq!(right.get_token().t_type, TokenType::Int);
//                 assert_eq!(right.get_token().t_value, Some(TokenValue::Numeric(5)));
//             } else {
//                 panic!("no right subexpression");
//             }
//         } else {
//             panic!("no inner expression");
//         }

//         let input = String::from("-15;");
//         let c = input.chars().collect();

//         let l = Lexer::new(&c);
//         let mut p = Parser::new(l);
//         let program = p.parse_program();

//         assert_eq!(program.statements.len(), 1);
//         let token = program.statements[0].get_token();
//         assert_eq!(token.t_type, TokenType::Minus);
//         let inner_expression = program.statements[0].get_expression();
//         if let Some(exp) = inner_expression {
//             let right = exp.get_right_subexpression();
//             if let Some(right) = right {
//                 assert_eq!(right.get_token().t_type, TokenType::Int);
//                 assert_eq!(right.get_token().t_value, Some(TokenValue::Numeric(15)));
//             } else {
//                 panic!("no right subexpression");
//             }
//         } else {
//             panic!("no inner expression");
//         }
//     }

//     #[test]
//     fn test_infix_expression() {
//         struct InfixTest {
//             input: String,
//             left_int: i32,
//             right_int: i32,
//             operator: Token,
//         }

//         let v = vec![
//             InfixTest {
//                 input: String::from("5+5;"),
//                 left_int: 5,
//                 right_int: 5,
//                 operator: Token {
//                     t_type: TokenType::Plus,
//                     t_value: None,
//                 },
//             },
//             InfixTest {
//                 input: String::from(" 5 < 6"),
//                 left_int: 5,
//                 right_int: 6,
//                 operator: Token {
//                     t_type: TokenType::LT,
//                     t_value: None,
//                 },
//             },
//             InfixTest {
//                 input: String::from("5 != 7  ;"),
//                 left_int: 5,
//                 right_int: 7,
//                 operator: Token {
//                     t_type: TokenType::NotEq,
//                     t_value: None,
//                 },
//             },
//             InfixTest {
//                 input: String::from("5 / 1"),
//                 left_int: 5,
//                 right_int: 1,
//                 operator: Token {
//                     t_type: TokenType::Slash,
//                     t_value: None,
//                 },
//             },
//         ];

//         for t in v.iter() {
//             let input = t.input.chars().collect();
//             let l = Lexer::new(&input);
//             let mut p = Parser::new(l);
//             let program = p.parse_program();
//             assert_eq!(program.statements.len(), 1);
//             let exp = program.statements[0].get_expression();
//             if let Some(exp) = exp {
//                 match (exp.get_left_subexpression(), exp.get_right_subexpression()) {
//                     (Some(e_left), Some(e_right)) => {
//                         assert_eq!(
//                             e_left.get_token().t_value,
//                             Some(TokenValue::Numeric(t.left_int))
//                         );
//                         assert_eq!(
//                             e_right.get_token().t_value,
//                             Some(TokenValue::Numeric(t.right_int))
//                         );
//                     }
//                     _ => panic!("inner expression missing"),
//                 }
//                 assert_eq!(exp.get_token(), t.operator);
//             } else {
//                 panic!("no inner expression");
//             }
//         }
//     }

//     #[test]
//     fn test_operator_precedence() {
//         struct PrecedenceTest {
//             input: Vec<char>,
//             output: String,
//         }

//         let v = vec![
//             PrecedenceTest {
//                 input: "-a * b".chars().collect(),
//                 output: String::from("((-a) * b)"),
//             },
//             PrecedenceTest {
//                 input: "a + b * c + d / e - f".chars().collect(),
//                 output: String::from("(((a + (b * c)) + (d / e)) - f)"),
//             },
//             PrecedenceTest {
//                 input: "3 + 4 * 5 == 3 * 1 + 4 * 5".chars().collect(),
//                 output: String::from("((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
//             },
//             PrecedenceTest {
//                 input: "2 - -3 * 4".chars().collect(),
//                 output: String::from("(2 - ((-3) * 4))"),
//             },
//             PrecedenceTest {
//                 input: "2 - 3 < 0 == true".chars().collect(),
//                 output: String::from("(((2 - 3) < 0) == true)"),
//             },
//             PrecedenceTest {
//                 input: "!(true == true)".chars().collect(),
//                 output: String::from("(!(true == true))"),
//             },
//             PrecedenceTest {
//                 input: "1 + (2 + 3) + 4".chars().collect(),
//                 output: String::from("((1 + (2 + 3)) + 4)"),
//             },
//         ];

//         for test in v.iter() {
//             let l = Lexer::new(&test.input);
//             let mut p = Parser::new(l);
//             let program = p.parse_program();
//             assert_eq!(test.output, program.string());
//         }
//     }

//     #[test]
//     fn test_if_expression() {
//         let input = String::from("if (x < y) { let x = 5; x } else { let x = 6; x");
//         let c = input.chars().collect();

//         let l = Lexer::new(&c);
//         let mut p = Parser::new(l);
//         let program = p.parse_program();

//         assert_eq!(program.statements.len(), 1);
//         let token = program.statements[0].get_token();
//         assert_eq!(token.t_type, TokenType::If);
//         if let Some(exp) = program.statements[0].get_expression() {
//             assert_eq!(
//                 "if (x < y) { let x = 5;\nx } else { let x = 6;\nx }",
//                 exp.string()
//             );
//         } else {
//             panic!("no inner expression");
//         }
//     }

//     #[test]
//     fn test_string_expression() {
//         let input = String::from(r#"let x = "hi"; x"#);
//         let c = input.chars().collect();

//         let l = Lexer::new(&c);
//         let mut p = Parser::new(l);
//         let program = Box::new(p.parse_program());

//         assert_eq!(program.statements.len(), 2);
//         let mut env = Box::new(Environment::new());
//         assert_eq!(String::from("hi"), program.eval(&mut env).inspect());
//     }
// }
