use crate::repl::code;
use crate::repl::code::{Instructions, OpCode, OP};
use crate::repl::lexer::{TokenType, TokenValue};
use crate::repl::object::Object;
use crate::repl::parser::{Expression, Program, Statement};

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            instructions: Instructions(Vec::new()),
            constants: Vec::new(),
        }
    }

    pub fn compile(mut self, p: Program) -> Bytecode {
        for statement in p.statements {
            self.compile_statement(*statement);
        }

        Bytecode {
            instructions: self.instructions,
            constants: self.constants,
        }
    }

    fn compile_statement(&mut self, s: Statement) {
        match s {
            Statement::Expression(exp) => {
                self.compile_expression(*exp);
                self.emit_instruction(OP::POP, &[]);
            }
            _ => {}
        }
    }

    fn compile_expression(&mut self, e: Expression) {
        match e {
            Expression::Prefix(token, right) => {
                self.compile_expression(*right);
                match token.t_type {
                    TokenType::Minus => self.emit_instruction(OP::NEG, &[]),
                    TokenType::Bang => self.emit_instruction(OP::NOT, &[]),
                    _ => {}
                }
            }
            Expression::Infix(left, token, right) => {
                // We're compiling for a stack-based virtual machine. That means
                // that an expression like 1 + 2 is ordered like 1|2|+ on the stack.
                match token.t_type {
                    TokenType::LT => {
                        // Swap the order of arguments and use the same op code
                        // for less than as for greater than.
                        self.compile_expression(*right);
                        self.compile_expression(*left);
                    }
                    _ => {
                        self.compile_expression(*left);
                        self.compile_expression(*right);
                    }
                }

                match token.t_type {
                    TokenType::Plus => self.emit_instruction(OP::ADD, &[]),
                    TokenType::Minus => self.emit_instruction(OP::SUB, &[]),
                    TokenType::Asterisk => self.emit_instruction(OP::MUL, &[]),
                    TokenType::Slash => self.emit_instruction(OP::DIV, &[]),
                    TokenType::NotEq => self.emit_instruction(OP::NE, &[]),
                    TokenType::Eq => self.emit_instruction(OP::EQ, &[]),
                    TokenType::GT => self.emit_instruction(OP::GT, &[]),
                    TokenType::LT => self.emit_instruction(OP::GT, &[]),
                    _ => {}
                }
            }
            Expression::Boolean(token) => match token.t_type {
                TokenType::True => self.emit_instruction(OP::TRUE, &[]),
                TokenType::False => self.emit_instruction(OP::FALSE, &[]),
                _ => {}
            },
            Expression::Integer(token) => {
                let obj = match token.t_value {
                    Some(TokenValue::Numeric(int)) => Object::Integer(int),
                    _ => Object::Null,
                };

                // Place the constant in the pool and emit instruction with the
                // address as the operand.
                let address = [self.add_constant(obj) as i32];
                self.emit_instruction(OP::CONSTANT, &address);
            }
            _ => {}
        }
    }

    // Method to add a constant to the constants pool
    fn add_constant(&mut self, o: Object) -> usize {
        self.constants.push(o);
        self.constants.len() - 1
    }

    // Returns the starting point of the added instruction
    fn emit_instruction(&mut self, op: OpCode, operands: &[i32]) {
        let ins = code::make(op, operands);
        let pos = self.add_instruction(ins);
        // TODO: do we need it? pos
    }

    // Returns the starting point of the added instruction
    fn add_instruction(&mut self, instructions: Instructions) -> usize {
        let pos = self.instructions.0.len();
        self.instructions.push_instructions(instructions);

        pos
    }
}
