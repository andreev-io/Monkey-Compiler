use crate::repl::lexer::{Token, TokenType, TokenValue};
use crate::repl::object::{Environment, Object};
use crate::repl::parser::{Expression, Program, Statement};

pub struct Evaluator {
    env: Environment,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            env: Environment::new(),
        }
    }

    pub fn new_with_environment(env: Environment) -> Evaluator {
        Evaluator { env }
    }

    pub fn eval_program(&mut self, program: Program) -> Object {
        let mut result = Object::Null;
        for mut statement in program.statements {
            result = self.eval_statement(&mut statement);

            match result {
                Object::ReturnValue(_) => {
                    return result;
                }
                _ => {}
            }
        }

        result
    }

    pub fn eval_statement(&mut self, statement: &mut Statement) -> Object {
        match statement {
            Statement::Block(statements) => self.eval_statements(statements),
            Statement::Let(id, expr) => self.eval_let(id, expr),
            Statement::Expression(expr) => self.eval_expression(expr),
            Statement::Return(expr) => Object::ReturnValue(Box::new(self.eval_expression(expr))),
            Statement::None => Object::Null,
        }
    }

    fn eval_statements(&mut self, statements: &mut Vec<Box<Statement>>) -> Object {
        let mut result = Object::Null;
        for statement in statements {
            result = self.eval_statement(statement);

            match result {
                Object::ReturnValue(_) => {
                    return result;
                }
                _ => {}
            }
        }

        result
    }

    fn eval_let(&mut self, id: &mut Token, expression: &mut Expression) -> Object {
        let val = self.eval_expression(expression);

        match &id.t_value {
            Some(TokenValue::Literal(name)) => {
                self.env.set(name.to_string(), val);
            }
            _ => {}
        };

        Object::Null
    }

    pub fn eval_expression(&mut self, expression: &mut Expression) -> Object {
        match expression {
            Expression::None => Object::Null,
            Expression::Index(array_expr, index_expr) => {
                let array = self.eval_expression(array_expr);
                let index = self.eval_expression(index_expr);
                match (array, index) {
                    (Object::Array(arr), Object::Integer(int)) => {
                        if int >= 0 && int < arr.len() as i32 {
                            *arr[int as usize].clone()
                        } else {
                            Object::Null
                        }
                    }
                    (_, _) => Object::Null,
                }
            }
            Expression::Call(func, args) => {
                let f = self.eval_expression(func);

                let mut res = Vec::new();
                for arg in args {
                    res.push(self.eval_expression(arg));
                }

                self.apply_function(f, res)
            }
            Expression::Array(expressions) => {
                let mut array = Vec::new();
                for expr in expressions {
                    array.push(Box::new(self.eval_expression(expr)));
                }

                Object::Array(array)
            }
            Expression::Infix(left, token, right) => {
                let (left, right) = (self.eval_expression(left), self.eval_expression(right));

                match (left, &token.t_type, right) {
                    (Object::Integer(l), TokenType::Plus, Object::Integer(r)) => {
                        Object::Integer(l + r)
                    }
                    (Object::Integer(l), TokenType::Minus, Object::Integer(r)) => {
                        Object::Integer(l - r)
                    }
                    (Object::Integer(l), TokenType::Asterisk, Object::Integer(r)) => {
                        Object::Integer(l * r)
                    }
                    (Object::Integer(l), TokenType::Slash, Object::Integer(r)) => {
                        Object::Integer(l / r)
                    }
                    (Object::Integer(l), TokenType::LT, Object::Integer(r)) => {
                        Object::Boolean(l < r)
                    }
                    (Object::Integer(l), TokenType::GT, Object::Integer(r)) => {
                        Object::Boolean(l > r)
                    }
                    (Object::Integer(l), TokenType::Eq, Object::Integer(r)) => {
                        Object::Boolean(l == r)
                    }
                    (Object::Integer(l), TokenType::NotEq, Object::Integer(r)) => {
                        Object::Boolean(l != r)
                    }
                    (Object::Boolean(l), TokenType::Eq, Object::Boolean(r)) => {
                        Object::Boolean(l == r)
                    }
                    (Object::Boolean(l), TokenType::NotEq, Object::Boolean(r)) => {
                        Object::Boolean(l != r)
                    }
                    (Object::String(l), TokenType::Plus, Object::String(r)) => {
                        Object::String(format!("{}{}", l, r))
                    }
                    (Object::String(l), TokenType::Eq, Object::String(r)) => {
                        Object::Boolean(l == r)
                    }
                    (Object::String(l), TokenType::NotEq, Object::String(r)) => {
                        Object::Boolean(l != r)
                    }
                    (_, _, _) => Object::Null,
                }
            }
            Expression::Prefix(token, expr) => {
                let right = self.eval_expression(expr);
                match (&token.t_type, right) {
                    (TokenType::Bang, Object::Boolean(b)) => Object::Boolean(!b),
                    (TokenType::Bang, Object::Null) => Object::Boolean(true),
                    (TokenType::Bang, _) => Object::Boolean(false),
                    (TokenType::Minus, Object::Integer(i)) => Object::Integer(-1 * i),
                    (_, _) => Object::Null,
                }
            }
            Expression::Boolean(token) => match token.t_type {
                TokenType::False => Object::Boolean(false),
                TokenType::True => Object::Boolean(true),
                _ => Object::Null,
            },
            Expression::Identifier(token) => {
                let ret = Object::Null;
                match &token.t_value {
                    Some(TokenValue::Literal(name)) => match self.env.get(name.to_string()) {
                        Some(obj) => obj.clone(),
                        _ => ret,
                    },
                    _ => ret,
                }
            }
            Expression::String(token) => match &token.t_value {
                Some(TokenValue::Literal(string)) => Object::String(string.to_string()),
                _ => Object::Null,
            },
            Expression::Integer(token) => match token.t_value {
                Some(TokenValue::Numeric(int)) => Object::Integer(int),
                _ => Object::Null,
            },
            Expression::If(condition, consequence, alternative) => {
                let cond = self.eval_expression(condition);
                let branch = match cond {
                    Object::Null | Object::Boolean(false) => false,
                    _ => true,
                };

                if branch {
                    self.eval_statement(consequence)
                } else {
                    self.eval_statement(alternative)
                }
            }
            Expression::Function(parameters, body) => {
                Object::Function(parameters.to_vec(), body.clone())
            }
        }
    }

    fn apply_function(&mut self, f: Object, args: Vec<Object>) -> Object {
        match f {
            Object::Function(params, mut body) => {
                let mut new_env = Environment::new();

                let iter = args.into_iter().zip(params);
                for (arg, param) in iter {
                    match param.t_value {
                        Some(TokenValue::Literal(name)) => new_env.set(name, arg),
                        _ => {}
                    }
                }

                let mut new_evaluator = Evaluator::new_with_environment(new_env);
                new_evaluator.eval_statement(&mut body)
            }
            _ => Object::Null,
        }
    }
}

// #[cfg(test)]
// mod test {
//     use crate::repl::{lexer::Lexer, object::Environment, object::Object, parser::Parser};
//     struct T {
//         input: Vec<char>,
//         answer: Object,
//         env: Box<Environment>,
//     }

//     #[test]
//     fn test_eval_integer_expression() {
//         let mut v = vec![
//             T {
//                 input: "5".chars().collect(),
//                 answer: Object::Integer(5),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "10".chars().collect(),
//                 answer: Object::Integer(10),
//                 env: Box::new(Environment::new()),
//             },
//         ];

//         for test in v.iter_mut() {
//             let l = Lexer::new(&test.input);
//             let mut p = Parser::new(l);
//             let program = Box::new(p.parse_program());
//             let output = program.eval(&mut test.env);
//             assert_eq!(output.inspect(), test.answer.inspect());
//         }
//     }

//     #[test]
//     fn test_eval_boolean_expression() {
//         let mut v = vec![
//             T {
//                 input: "true".chars().collect(),
//                 answer: Object::Boolean(true),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "false".chars().collect(),
//                 answer: Object::Boolean(false),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "1 < 2".chars().collect(),
//                 answer: Object::Boolean(true),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "1 != 2".chars().collect(),
//                 answer: Object::Boolean(true),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "1 == 2".chars().collect(),
//                 answer: Object::Boolean(false),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "(1 < 2) == true".chars().collect(),
//                 answer: Object::Boolean(true),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "(1 > 2) == false".chars().collect(),
//                 answer: Object::Boolean(true),
//                 env: Box::new(Environment::new()),
//             },
//         ];

//         for test in v.iter_mut() {
//             let l = Lexer::new(&test.input);
//             let mut p = Parser::new(l);
//             let program = Box::new(p.parse_program());
//             let output = program.eval(&mut test.env);
//             assert_eq!(output.inspect(), test.answer.inspect());
//         }
//     }

//     #[test]
//     fn test_eval_prefix_expression() {
//         let mut v = vec![
//             T {
//                 input: "!!5".chars().collect(),
//                 answer: Object::Boolean(true),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "!5".chars().collect(),
//                 answer: Object::Boolean(false),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "!false".chars().collect(),
//                 answer: Object::Boolean(true),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "!!true".chars().collect(),
//                 answer: Object::Boolean(true),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "-5".chars().collect(),
//                 answer: Object::Integer(-5),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "-10".chars().collect(),
//                 answer: Object::Integer(-10),
//                 env: Box::new(Environment::new()),
//             },
//         ];

//         for test in v.iter_mut() {
//             let l = Lexer::new(&test.input);
//             let mut p = Parser::new(l);
//             let program = Box::new(p.parse_program());
//             let output = program.eval(&mut test.env);
//             assert_eq!(output.inspect(), test.answer.inspect());
//         }
//     }

//     #[test]
//     fn test_eval_infix_expression() {
//         let mut v = vec![
//             T {
//                 input: "5 + 5 + 5 + 5 - 10".chars().collect(),
//                 answer: Object::Integer(10),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "2 * 2 * 2 * 2 * 2".chars().collect(),
//                 answer: Object::Integer(32),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "3 * (3 * 3) + 10".chars().collect(),
//                 answer: Object::Integer(37),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".chars().collect(),
//                 answer: Object::Integer(50),
//                 env: Box::new(Environment::new()),
//             },
//         ];

//         for test in v.iter_mut() {
//             let l = Lexer::new(&test.input);
//             let mut p = Parser::new(l);
//             let program = Box::new(p.parse_program());
//             let output = program.eval(&mut test.env);
//             assert_eq!(output.inspect(), test.answer.inspect());
//         }
//     }

//     #[test]
//     fn test_if_else_expressions() {
//         let mut v = vec![
//             T {
//                 input: "if (true) { 10 }".chars().collect(),
//                 answer: Object::Integer(10),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "if (1 < 2) { 10 } else { 20 }".chars().collect(),
//                 answer: Object::Integer(10),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "if (1 > 2) { 10 } else { 20 }".chars().collect(),
//                 answer: Object::Integer(20),
//                 env: Box::new(Environment::new()),
//             },
//         ];

//         for test in v.iter_mut() {
//             let l = Lexer::new(&test.input);
//             let mut p = Parser::new(l);
//             let program = Box::new(p.parse_program());
//             let output = program.eval(&mut test.env);
//             assert_eq!(output.inspect(), test.answer.inspect());
//         }
//     }

//     #[test]
//     fn test_return_statements() {
//         let mut v = vec![
//             T {
//                 input: "return 10;".chars().collect(),
//                 answer: Object::Integer(10),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "return 10; 9".chars().collect(),
//                 answer: Object::Integer(10),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "9; return 2 * 5; 9;".chars().collect(),
//                 answer: Object::Integer(10),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: r#"if (10 > 1) {
//                     if (10 > 1) {
//                       return 10;
//                }
//                return 1; }"#
//                     .chars()
//                     .collect(),
//                 answer: Object::Integer(10),
//                 env: Box::new(Environment::new()),
//             },
//         ];

//         for test in v.iter_mut() {
//             let l = Lexer::new(&test.input);
//             let mut p = Parser::new(l);
//             let program = Box::new(p.parse_program());
//             let output = program.eval(&mut test.env);
//             assert_eq!(output.inspect(), test.answer.inspect());
//         }
//     }

//     #[test]
//     fn test_let_statements() {
//         let mut v = vec![
//             T {
//                 input: "let a = 5; a;".chars().collect(),
//                 answer: Object::Integer(5),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "let a = 5; let b = 15; let c = a + b; c;".chars().collect(),
//                 answer: Object::Integer(20),
//                 env: Box::new(Environment::new()),
//             },
//         ];

//         for test in v.iter_mut() {
//             let l = Lexer::new(&test.input);
//             let mut p = Parser::new(l);
//             let program = Box::new(p.parse_program());
//             let output = program.eval(&mut test.env);
//             assert_eq!(output.inspect(), test.answer.inspect());
//         }
//     }

//     #[test]
//     fn test_call_expressions() {
//         let mut v = vec![
//             T {
//                 input: "let identity = fn(x) { x; }; identity(5);"
//                     .chars()
//                     .collect(),
//                 answer: Object::Integer(5),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "let identity = fn(x) { return x; }; identity(5);"
//                     .chars()
//                     .collect(),
//                 answer: Object::Integer(5),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "let double = fn(x) { x * 2; }; double(5);"
//                     .chars()
//                     .collect(),
//                 answer: Object::Integer(10),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "let add = fn(x, y) { x + y; }; add(5, 5);"
//                     .chars()
//                     .collect(),
//                 answer: Object::Integer(10),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: "fn(x) { x; }(5)".chars().collect(),
//                 answer: Object::Integer(5),
//                 env: Box::new(Environment::new()),
//             },
//         ];

//         for test in v.iter_mut() {
//             let l = Lexer::new(&test.input);
//             let mut p = Parser::new(l);
//             let program = Box::new(p.parse_program());
//             let output = program.eval(&mut test.env);
//             assert_eq!(output.inspect(), test.answer.inspect());
//         }
//     }

//     #[test]
//     fn test_string_expressions() {
//         let mut v = vec![T {
//             input: r#"let x = "good"; let y = "bye"; if (x+y == "goodbye") { 1 } else { 0 }"#
//                 .chars()
//                 .collect(),
//             answer: Object::Integer(1),
//             env: Box::new(Environment::new()),
//         }];

//         for test in v.iter_mut() {
//             let l = Lexer::new(&test.input);
//             let mut p = Parser::new(l);
//             let program = Box::new(p.parse_program());
//             let output = program.eval(&mut test.env);
//             assert_eq!(output.inspect(), test.answer.inspect());
//         }
//     }

//     #[test]
//     fn test_array_expressions() {
//         let mut v = vec![
//             T {
//                 input: r#"let x = [1, 2, fn(x) { x }(5)]; x[2]"#.chars().collect(),
//                 answer: Object::Integer(5),
//                 env: Box::new(Environment::new()),
//             },
//             T {
//                 input: r#"[1, 2, fn(x) { x }(5)][3-2]"#.chars().collect(),
//                 answer: Object::Integer(2),
//                 env: Box::new(Environment::new()),
//             },
//         ];

//         for test in v.iter_mut() {
//             let l = Lexer::new(&test.input);
//             let mut p = Parser::new(l);
//             let program = Box::new(p.parse_program());
//             let output = program.eval(&mut test.env);
//             assert_eq!(output.inspect(), test.answer.inspect());
//         }
//     }
// }
