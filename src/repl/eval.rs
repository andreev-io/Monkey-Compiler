use crate::repl::lexer::{Token, TokenType, TokenValue};
use crate::repl::object::{Environment, Object};
use crate::repl::parser::{Expression, Program, Statement};
use core::cell::RefCell;
use std::rc::Rc;

pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    fn new_with_environment(env: Rc<RefCell<Environment>>) -> Evaluator {
        Evaluator { env }
    }

    pub fn eval_program(&mut self, program: Program) -> Object {
        let mut result = Object::Null;
        for mut statement in program.statements {
            result = self.eval_statement(&mut statement);

            match result {
                Object::ReturnValue(res) => {
                    return *res;
                }
                _ => {}
            }
        }

        result
    }

    fn eval_statement(&mut self, statement: &mut Statement) -> Object {
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
                self.env.borrow_mut().set(name.to_string(), val);
            }
            _ => {}
        };

        Object::Null
    }

    fn eval_expression(&mut self, expression: &mut Expression) -> Object {
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
                for arg in args.iter_mut() {
                    res.push(self.eval_expression(arg));
                }

                let ret = self.apply_function(f, res);
                match ret {
                    Object::ReturnValue(value) => *value,
                    _ => ret,
                }
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
                match (&token.t_type) {
                    TokenType::Plus => left + right,
                    TokenType::Minus => left - right,
                    TokenType::Asterisk => left * right,
                    TokenType::Slash => left / right,
                    TokenType::LT => Object::Boolean(left > right),
                    TokenType::GT => Object::Boolean(left < right),
                    TokenType::Eq => Object::Boolean(left == right),
                    TokenType::NotEq => Object::Boolean(left != right),
                    _ => Object::Null,
                }
            }
            Expression::Prefix(token, expr) => {
                let right = self.eval_expression(expr);
                match &token.t_type {
                    TokenType::Bang => !right,
                    TokenType::Minus => -right,
                    _ => Object::Null,
                }
            }
            Expression::Boolean(token) => match token.t_type {
                TokenType::False => Object::Boolean(false),
                TokenType::True => Object::Boolean(true),
                _ => Object::Null,
            },
            Expression::Identifier(token) => match &token.t_value {
                Some(TokenValue::Literal(name)) => {
                    match self.env.borrow_mut().get(name.to_string()) {
                        Some(obj) => obj,
                        _ => Object::Null,
                    }
                }
                _ => Object::Null,
            },
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
                Object::Function(parameters.to_vec(), body.clone(), self.env.clone())
            }
        }
    }

    fn apply_function(&mut self, f: Object, args: Vec<Object>) -> Object {
        match f {
            Object::Function(params, mut body, scope) => {
                let mut new_env = Environment::new_enclosed(scope);

                let iter = args.into_iter().zip(params);
                for (arg, param) in iter {
                    match param.t_value {
                        Some(TokenValue::Literal(name)) => new_env.set(name, arg),
                        _ => {}
                    }
                }

                let mut new_evaluator =
                    Evaluator::new_with_environment(Rc::new(RefCell::new(new_env)));
                new_evaluator.eval_statement(&mut body)
            }
            _ => Object::Null,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::repl::{eval::Evaluator, lexer::Lexer, object::Object, parser::Parser};
    struct T {
        input: Vec<char>,
        answer: Object,
        eval: Evaluator,
    }

    #[test]
    fn test_eval_integer_expression() {
        let mut v = vec![
            T {
                input: "5".chars().collect(),
                answer: Object::Integer(5),
                eval: Evaluator::new(),
            },
            T {
                input: "10".chars().collect(),
                answer: Object::Integer(10),
                eval: Evaluator::new(),
            },
        ];

        for test in v.iter_mut() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            let output = test.eval.eval_program(program).inspect();
            assert_eq!(output, test.answer.inspect());
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let mut v = vec![
            T {
                input: "true".chars().collect(),
                answer: Object::Boolean(true),
                eval: Evaluator::new(),
            },
            T {
                input: "false".chars().collect(),
                answer: Object::Boolean(false),
                eval: Evaluator::new(),
            },
            T {
                input: "1 < 2".chars().collect(),
                answer: Object::Boolean(true),
                eval: Evaluator::new(),
            },
            T {
                input: "1 != 2".chars().collect(),
                answer: Object::Boolean(true),
                eval: Evaluator::new(),
            },
            T {
                input: "1 == 2".chars().collect(),
                answer: Object::Boolean(false),
                eval: Evaluator::new(),
            },
            T {
                input: "(1 < 2) == true".chars().collect(),
                answer: Object::Boolean(true),
                eval: Evaluator::new(),
            },
            T {
                input: "(1 > 2) == false".chars().collect(),
                answer: Object::Boolean(true),
                eval: Evaluator::new(),
            },
        ];

        for test in v.iter_mut() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            let output = test.eval.eval_program(program).inspect();
            assert_eq!(output, test.answer.inspect());
        }
    }

    #[test]
    fn test_eval_prefix_expression() {
        let mut v = vec![
            T {
                input: "!!5".chars().collect(),
                answer: Object::Boolean(true),
                eval: Evaluator::new(),
            },
            T {
                input: "!5".chars().collect(),
                answer: Object::Boolean(false),
                eval: Evaluator::new(),
            },
            T {
                input: "!false".chars().collect(),
                answer: Object::Boolean(true),
                eval: Evaluator::new(),
            },
            T {
                input: "!!true".chars().collect(),
                answer: Object::Boolean(true),
                eval: Evaluator::new(),
            },
            T {
                input: "-5".chars().collect(),
                answer: Object::Integer(-5),
                eval: Evaluator::new(),
            },
            T {
                input: "-10".chars().collect(),
                answer: Object::Integer(-10),
                eval: Evaluator::new(),
            },
        ];

        for test in v.iter_mut() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            let output = test.eval.eval_program(program).inspect();
            assert_eq!(output, test.answer.inspect());
        }
    }

    #[test]
    fn test_recursion() {
        let input = "let fibonacci = fn(x) { if (x == 0) {
            0
            } else {
            if (x == 1) {
            return 1; } else {
                     fibonacci(x - 1) + fibonacci(x - 2);
                   }
            } };
            
            fibonacci(7)"
            .chars()
            .collect();
        let l = Lexer::new(&input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let output = Evaluator::new().eval_program(program).inspect();
        assert_eq!(output, String::from("13"));
    }

    #[test]
    fn test_eval_infix_expression() {
        let mut v = vec![
            T {
                input: "5 + 5 + 5 + 5 - 10".chars().collect(),
                answer: Object::Integer(10),
                eval: Evaluator::new(),
            },
            T {
                input: "2 * 2 * 2 * 2 * 2".chars().collect(),
                answer: Object::Integer(32),
                eval: Evaluator::new(),
            },
            T {
                input: "3 * (3 * 3) + 10".chars().collect(),
                answer: Object::Integer(37),
                eval: Evaluator::new(),
            },
            T {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".chars().collect(),
                answer: Object::Integer(50),
                eval: Evaluator::new(),
            },
        ];

        for test in v.iter_mut() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            let output = test.eval.eval_program(program).inspect();
            assert_eq!(output, test.answer.inspect());
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let mut v = vec![
            T {
                input: "if (true) { 10 }".chars().collect(),
                answer: Object::Integer(10),
                eval: Evaluator::new(),
            },
            T {
                input: "if (1 < 2) { 10 } else { 20 }".chars().collect(),
                answer: Object::Integer(10),
                eval: Evaluator::new(),
            },
            T {
                input: "if (1 > 2) { 10 } else { 20 }".chars().collect(),
                answer: Object::Integer(20),
                eval: Evaluator::new(),
            },
            T {
                input: "let x = 5; if (x > 2) { 10 } else { 20 }".chars().collect(),
                answer: Object::Integer(10),
                eval: Evaluator::new(),
            },
            T {
                input: "let x = 5; if (x == 0) { 1 } else { x * (x-1) }"
                    .chars()
                    .collect(),
                answer: Object::Integer(20),
                eval: Evaluator::new(),
            },
        ];

        for test in v.iter_mut() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            let output = test.eval.eval_program(program).inspect();
            assert_eq!(output, test.answer.inspect());
        }
    }

    #[test]
    fn test_return_statements() {
        let mut v = vec![
            T {
                input: "return 10;".chars().collect(),
                answer: Object::Integer(10),
                eval: Evaluator::new(),
            },
            T {
                input: "return 10; 9".chars().collect(),
                answer: Object::Integer(10),
                eval: Evaluator::new(),
            },
            T {
                input: "9; return 2 * 5; 9;".chars().collect(),
                answer: Object::Integer(10),
                eval: Evaluator::new(),
            },
            T {
                input: r#"if (10 > 1) {
                    if (10 > 1) {
                      return 10;
               }
               return 1; }"#
                    .chars()
                    .collect(),
                answer: Object::Integer(10),
                eval: Evaluator::new(),
            },
        ];

        for test in v.iter_mut() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            let output = test.eval.eval_program(program).inspect();
            assert_eq!(output, test.answer.inspect());
        }
    }

    #[test]
    fn test_let_statements() {
        let mut v = vec![
            T {
                input: "let a = 5; a;".chars().collect(),
                answer: Object::Integer(5),
                eval: Evaluator::new(),
            },
            T {
                input: "let a = 5; let b = 15; let c = a + b; c;".chars().collect(),
                answer: Object::Integer(20),
                eval: Evaluator::new(),
            },
        ];

        for test in v.iter_mut() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            let output = test.eval.eval_program(program).inspect();
            assert_eq!(output, test.answer.inspect());
        }
    }

    #[test]
    fn test_call_expressions() {
        let mut v = vec![
            T {
                input: "let identity = fn(x) { x; }; identity(5);"
                    .chars()
                    .collect(),
                answer: Object::Integer(5),
                eval: Evaluator::new(),
            },
            T {
                input: "let identity = fn(x) { return x; }; identity(5);"
                    .chars()
                    .collect(),
                answer: Object::Integer(5),
                eval: Evaluator::new(),
            },
            T {
                input: "let double = fn(x) { x * 2; }; double(5);"
                    .chars()
                    .collect(),
                answer: Object::Integer(10),
                eval: Evaluator::new(),
            },
            T {
                input: "let add = fn(x, y) { x + y; }; add(5, 5);"
                    .chars()
                    .collect(),
                answer: Object::Integer(10),
                eval: Evaluator::new(),
            },
            T {
                input: "fn(x) { x; }(5)".chars().collect(),
                answer: Object::Integer(5),
                eval: Evaluator::new(),
            },
        ];

        for test in v.iter_mut() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            let output = test.eval.eval_program(program).inspect();
            assert_eq!(output, test.answer.inspect());
        }
    }

    #[test]
    fn test_string_expressions() {
        let mut v = vec![T {
            input: r#"let x = "good"; let y = "bye"; if (x+y == "goodbye") { 1 } else { 0 }"#
                .chars()
                .collect(),
            answer: Object::Integer(1),
            eval: Evaluator::new(),
        }];

        for test in v.iter_mut() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            let output = test.eval.eval_program(program).inspect();
            assert_eq!(output, test.answer.inspect());
        }
    }

    #[test]
    fn test_array_expressions() {
        let mut v = vec![
            T {
                input: r#"let x = [1, 2, fn(x) { x }(5)]; x[2]"#.chars().collect(),
                answer: Object::Integer(5),
                eval: Evaluator::new(),
            },
            T {
                input: r#"[1, 2, fn(x) { x }(5)][3-2]"#.chars().collect(),
                answer: Object::Integer(2),
                eval: Evaluator::new(),
            },
        ];

        for test in v.iter_mut() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            let output = test.eval.eval_program(program).inspect();
            assert_eq!(output, test.answer.inspect());
        }
    }
}
