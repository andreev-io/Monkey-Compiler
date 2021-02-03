use crate::repl::code;
use crate::repl::code::{Instructions, OpCode, OP};
use crate::repl::lexer::{TokenType, TokenValue};
use crate::repl::object::Object;
use crate::repl::parser::{Expression, Program, Statement};
use std::collections::HashMap;
use std::convert::TryInto;

#[derive(Clone)]
struct EmittedInstruction {
    op: OpCode,
    pos: usize,
}

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,

    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,

    pub symbol_table: SymbolTable,
}

#[derive(Debug)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

enum SymbolScope {
    Global,
}

struct Symbol {
    name: String,
    scope: SymbolScope,
    index: usize,
}

pub struct SymbolTable(HashMap<String, Symbol>);

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable(HashMap::new())
    }

    fn define(&mut self, name: String) -> usize {
        let symbol = Symbol {
            name: name.clone(),
            scope: SymbolScope::Global,
            index: self.0.len(),
        };

        let symbol_index = symbol.index;

        self.0.insert(name.clone(), symbol);
        symbol_index
    }

    fn resolve(&mut self, name: String) -> Option<usize> {
        if let Some(symbol) = self.0.get(&name) {
            Some(symbol.index)
        } else {
            None
        }
    }
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            instructions: Instructions(Vec::new()),
            constants: Vec::new(),

            last_instruction: None,
            previous_instruction: None,

            symbol_table: SymbolTable::new(),
        }
    }

    pub fn new_with_state(s: SymbolTable, constants: Vec<Object>) -> Compiler {
        let mut c = Compiler::new();
        c.symbol_table = s;
        c.constants = constants;
        c
    }

    pub fn compile(mut self, p: Program) -> (Bytecode, SymbolTable) {
        for statement in p.statements {
            self.compile_statement(*statement);
        }

        (
            Bytecode {
                instructions: self.instructions,
                constants: self.constants,
            },
            self.symbol_table,
        )
    }

    fn compile_statement(&mut self, s: Statement) {
        match s {
            Statement::Let(token, exp) => {
                self.compile_expression(*exp);

                match token.t_value.unwrap() {
                    TokenValue::Literal(name) => {
                        let symbol_index = self.symbol_table.define(name);
                        self.emit_instruction(OP::SET_GLOB, &[symbol_index as i32]);
                    }
                    _ => {}
                }
            }
            Statement::Expression(exp) => {
                self.compile_expression(*exp);
                self.emit_instruction(OP::POP, &[]);
            }
            Statement::Block(statements) => {
                for statement in statements {
                    self.compile_statement(*statement);
                }
            }
            _ => {}
        }
    }

    fn compile_expression(&mut self, e: Expression) {
        match e {
            Expression::Index(arr, idx) => {
                self.compile_expression(*arr);
                self.compile_expression(*idx);
                self.emit_instruction(OP::IDX, &[]);
            }
            Expression::Array(arr) => {
                let len = arr.len();
                for exp in arr {
                    self.compile_expression(*exp);
                }

                self.emit_instruction(OP::ARR, &[len as i32]);
            }
            Expression::String(token) => {
                let obj = match token.t_value {
                    Some(TokenValue::Literal(string)) => Object::String(string),
                    _ => Object::Null,
                };

                let address = [self.add_constant(obj) as i32];
                self.emit_instruction(OP::CONSTANT, &address);
            }
            Expression::Identifier(token) => match token.t_value.unwrap() {
                TokenValue::Literal(name) => {
                    if let Some(symbol_index) = self.symbol_table.resolve(name.clone()) {
                        self.emit_instruction(OP::GET_GLOB, &[symbol_index as i32]);
                    } else {
                        panic!("undefined variable {}", name);
                    }
                }
                _ => {}
            },
            Expression::If(condition, consequence, alternative) => {
                self.compile_expression(*condition);

                // Emit the conditional jump instruction with a placeholder
                // offset.
                let placeholder_offset = self.emit_instruction(OP::JMP_IF_NOT, &[6666]);

                // Compile the consequence block statement. Subtle issue:
                // consider the code if (true) { 1; 2; 3; }. Here, 1; 2; 3; are
                // expression statements, which have a POP instruction added
                // after them to avoid bloating the stack. However, the above if
                // block is also an expression and should return 3, so the last
                // POP instruction needs to be removed.
                self.compile_statement(*consequence);
                if self.last_instruction.as_ref().unwrap().op == OP::POP {
                    self.instructions.0.pop();
                    // Note: here we don't reconstruct the instruction that was
                    // prior to the previous_instruction.
                    self.last_instruction = self.previous_instruction.clone();
                }

                let jump_offset = self.emit_instruction(OP::JMP, &[6666]);

                // TODO: merge code into a separate backfilling function
                match *alternative {
                    // If there is no else branch, we can fix the value of the
                    // conditional jump.
                    Statement::None => {
                        // As a single-pass compiler, we now need to backfill the
                        // correct address for the conditional jump.
                        let after_conseq_offset = self.instructions.0.len();
                        // Get the conditional jump op code.
                        let op = self.instructions.0[placeholder_offset];
                        // Create a fixed conditional jump instruction.
                        let new_cnd_jmp =
                            code::make(op, &[after_conseq_offset.try_into().unwrap()]);
                        // Replace the placeholder instruction.
                        self.instructions
                            .replace_at_offset(placeholder_offset, new_cnd_jmp);

                        // If is an expression, so an empty alternative branch
                        // needs to evaluate to null.
                        self.emit_instruction(OP::SET_NULL, &[]);
                    }
                    // In this branch, we first place the placeholder
                    // unconditional jump for the consequence block to jump over
                    // the alternative block. Once that's placed, we can then
                    // update the placeholder conditional jump to the address of
                    // the alternative block.
                    _ => {
                        let after_conseq_offset = self.instructions.0.len();
                        // Get the conditional jump op code.
                        let op = self.instructions.0[placeholder_offset];
                        // Create a fixed conditional jump instruction.
                        let new_cnd_jmp =
                            code::make(op, &[after_conseq_offset.try_into().unwrap()]);
                        // Replace the placeholder instruction.
                        self.instructions
                            .replace_at_offset(placeholder_offset, new_cnd_jmp);

                        self.compile_statement(*alternative);
                        if self.last_instruction.as_ref().unwrap().op == OP::POP {
                            self.instructions.0.pop();
                            // Note: here we don't reconstruct the instruction that was
                            // prior to the previous_instruction.
                            self.last_instruction = self.previous_instruction.clone();
                        }
                    }
                }

                let after_alternative_offset = self.instructions.0.len();
                // Get the jump op code.
                let op = self.instructions.0[jump_offset];
                // Create a fixed conditional jump instruction.
                let new_jmp = code::make(op, &[after_alternative_offset.try_into().unwrap()]);
                // Replace the placeholder instruction.
                self.instructions.replace_at_offset(jump_offset, new_jmp);
            }
            Expression::Prefix(token, right) => {
                self.compile_expression(*right);
                match token.t_type {
                    TokenType::Minus => {
                        self.emit_instruction(OP::NEG, &[]);
                    }
                    TokenType::Bang => {
                        self.emit_instruction(OP::NOT, &[]);
                    }
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
                    TokenType::Plus => {
                        self.emit_instruction(OP::ADD, &[]);
                    }
                    TokenType::Minus => {
                        self.emit_instruction(OP::SUB, &[]);
                    }
                    TokenType::Asterisk => {
                        self.emit_instruction(OP::MUL, &[]);
                    }
                    TokenType::Slash => {
                        self.emit_instruction(OP::DIV, &[]);
                    }
                    TokenType::NotEq => {
                        self.emit_instruction(OP::NE, &[]);
                    }
                    TokenType::Eq => {
                        self.emit_instruction(OP::EQ, &[]);
                    }
                    TokenType::GT => {
                        self.emit_instruction(OP::GT, &[]);
                    }
                    TokenType::LT => {
                        self.emit_instruction(OP::GT, &[]);
                    }
                    _ => {}
                }
            }
            Expression::Boolean(token) => match token.t_type {
                TokenType::True => {
                    self.emit_instruction(OP::TRUE, &[]);
                }
                TokenType::False => {
                    self.emit_instruction(OP::FALSE, &[]);
                }
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
    fn emit_instruction(&mut self, op: OpCode, operands: &[i32]) -> usize {
        let ins = code::make(op, operands);
        let pos = self.add_instruction(ins);

        self.previous_instruction = self.last_instruction.clone();
        self.last_instruction = Some(EmittedInstruction { op, pos });

        pos
    }

    // Returns the starting point of the added instruction
    fn add_instruction(&mut self, instructions: Instructions) -> usize {
        let pos = self.instructions.0.len();
        self.instructions.push_instructions(instructions);

        pos
    }
}
