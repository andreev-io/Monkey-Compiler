#[cfg(test)]
mod test {
    use crate::repl::object::Node;
    use crate::repl::{lexer::Lexer, object::Environment, object::Object, parser::Parser};
    struct T {
        input: Vec<char>,
        answer: Object,
        env: Box<Environment>,
    }

    #[test]
    fn test_eval_integer_expression() {
        let mut v = vec![
            T {
                input: "5".chars().collect(),
                answer: Object::Integer(5),
                env: Box::new(Environment::new()),
            },
            T {
                input: "10".chars().collect(),
                answer: Object::Integer(10),
                env: Box::new(Environment::new()),
            },
        ];

        for test in v.iter_mut() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = Box::new(p.parse_program());
            let output = program.eval(&mut test.env);
            assert_eq!(output.inspect(), test.answer.inspect());
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let mut v = vec![
            T {
                input: "true".chars().collect(),
                answer: Object::Boolean(true),
                env: Box::new(Environment::new()),
            },
            T {
                input: "false".chars().collect(),
                answer: Object::Boolean(false),
                env: Box::new(Environment::new()),
            },
            T {
                input: "1 < 2".chars().collect(),
                answer: Object::Boolean(true),
                env: Box::new(Environment::new()),
            },
            T {
                input: "1 != 2".chars().collect(),
                answer: Object::Boolean(true),
                env: Box::new(Environment::new()),
            },
            T {
                input: "1 == 2".chars().collect(),
                answer: Object::Boolean(false),
                env: Box::new(Environment::new()),
            },
            T {
                input: "(1 < 2) == true".chars().collect(),
                answer: Object::Boolean(true),
                env: Box::new(Environment::new()),
            },
            T {
                input: "(1 > 2) == false".chars().collect(),
                answer: Object::Boolean(true),
                env: Box::new(Environment::new()),
            },
        ];

        for test in v.iter_mut() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = Box::new(p.parse_program());
            let output = program.eval(&mut test.env);
            assert_eq!(output.inspect(), test.answer.inspect());
        }
    }

    #[test]
    fn test_eval_prefix_expression() {
        let mut v = vec![
            T {
                input: "!!5".chars().collect(),
                answer: Object::Boolean(true),
                env: Box::new(Environment::new()),
            },
            T {
                input: "!5".chars().collect(),
                answer: Object::Boolean(false),
                env: Box::new(Environment::new()),
            },
            T {
                input: "!false".chars().collect(),
                answer: Object::Boolean(true),
                env: Box::new(Environment::new()),
            },
            T {
                input: "!!true".chars().collect(),
                answer: Object::Boolean(true),
                env: Box::new(Environment::new()),
            },
            T {
                input: "-5".chars().collect(),
                answer: Object::Integer(-5),
                env: Box::new(Environment::new()),
            },
            T {
                input: "-10".chars().collect(),
                answer: Object::Integer(-10),
                env: Box::new(Environment::new()),
            },
        ];

        for test in v.iter_mut() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = Box::new(p.parse_program());
            let output = program.eval(&mut test.env);
            assert_eq!(output.inspect(), test.answer.inspect());
        }
    }

    #[test]
    fn test_eval_infix_expression() {
        let mut v = vec![
            T {
                input: "5 + 5 + 5 + 5 - 10".chars().collect(),
                answer: Object::Integer(10),
                env: Box::new(Environment::new()),
            },
            T {
                input: "2 * 2 * 2 * 2 * 2".chars().collect(),
                answer: Object::Integer(32),
                env: Box::new(Environment::new()),
            },
            T {
                input: "3 * (3 * 3) + 10".chars().collect(),
                answer: Object::Integer(37),
                env: Box::new(Environment::new()),
            },
            T {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".chars().collect(),
                answer: Object::Integer(50),
                env: Box::new(Environment::new()),
            },
        ];

        for test in v.iter_mut() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = Box::new(p.parse_program());
            let output = program.eval(&mut test.env);
            assert_eq!(output.inspect(), test.answer.inspect());
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let mut v = vec![
            T {
                input: "if (true) { 10 }".chars().collect(),
                answer: Object::Integer(10),
                env: Box::new(Environment::new()),
            },
            T {
                input: "if (1 < 2) { 10 } else { 20 }".chars().collect(),
                answer: Object::Integer(10),
                env: Box::new(Environment::new()),
            },
            T {
                input: "if (1 > 2) { 10 } else { 20 }".chars().collect(),
                answer: Object::Integer(20),
                env: Box::new(Environment::new()),
            },
        ];

        for test in v.iter_mut() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = Box::new(p.parse_program());
            let output = program.eval(&mut test.env);
            assert_eq!(output.inspect(), test.answer.inspect());
        }
    }

    #[test]
    fn test_return_statements() {
        let mut v = vec![
            T {
                input: "return 10;".chars().collect(),
                answer: Object::Integer(10),
                env: Box::new(Environment::new()),
            },
            T {
                input: "return 10; 9".chars().collect(),
                answer: Object::Integer(10),
                env: Box::new(Environment::new()),
            },
            T {
                input: "9; return 2 * 5; 9;".chars().collect(),
                answer: Object::Integer(10),
                env: Box::new(Environment::new()),
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
                env: Box::new(Environment::new()),
            },
        ];

        for test in v.iter_mut() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = Box::new(p.parse_program());
            let output = program.eval(&mut test.env);
            assert_eq!(output.inspect(), test.answer.inspect());
        }
    }

    #[test]
    fn test_let_statements() {
        let mut v = vec![
            T {
                input: "let a = 5; a;".chars().collect(),
                answer: Object::Integer(5),
                env: Box::new(Environment::new()),
            },
            T {
                input: "let a = 5; let b = a; let c = a + b + 5; c;"
                    .chars()
                    .collect(),
                answer: Object::Integer(15),
                env: Box::new(Environment::new()),
            },
        ];

        for test in v.iter_mut() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = Box::new(p.parse_program());
            let output = program.eval(&mut test.env);
            assert_eq!(output.inspect(), test.answer.inspect());
        }
    }
}
