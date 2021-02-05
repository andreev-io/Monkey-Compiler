use crate::repl::code;
use crate::repl::code::{Instructions, OpCode, OP};
use crate::repl::lexer::{TokenType, TokenValue};
use crate::repl::object::Object;
use crate::repl::parser::{Expression, Program, Statement};
use core::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;

#[derive(Clone)]
struct EmittedInstruction {
    op: OpCode,
    pos: usize,
}

struct CompilationScope {
    instructions: Instructions,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

pub struct Compiler {
    constants: Vec<Object>,

    pub symbol_table: SymbolTable,

    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

#[derive(Debug)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[derive(Clone, Debug)]
enum SymbolScope {
    Global,
    Local,
}

#[derive(Clone)]
struct Symbol {
    name: String,
    scope: SymbolScope,
    index: usize,
}

#[derive(Clone)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    outer: Option<Box<SymbolTable>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(s: SymbolTable) -> SymbolTable {
        SymbolTable {
            store: HashMap::new(),
            outer: Some(Box::new(s)),
        }
    }

    fn define(&mut self, name: String) -> (usize, SymbolScope) {
        let scope = if let Some(_) = &self.outer {
            SymbolScope::Local
        } else {
            SymbolScope::Global
        };

        let symbol = Symbol {
            name: name.clone(),
            scope: scope.clone(),
            index: self.store.len(),
        };

        let symbol_index = symbol.index;

        self.store.insert(name, symbol);
        (symbol_index, scope)
    }

    fn resolve(&self, name: String) -> Option<(usize, SymbolScope)> {
        if let Some(symbol) = self.store.get(&name) {
            Some((symbol.index, symbol.scope.clone()))
        } else {
            if let Some(outer) = &self.outer {
                return outer.resolve(name);
            }

            None
        }
    }
}

impl Compiler {
    pub fn new() -> Compiler {
        let main_scope = CompilationScope {
            instructions: Instructions(Vec::new()),
            last_instruction: None,
            previous_instruction: None,
        };

        Compiler {
            constants: Vec::new(),
            symbol_table: SymbolTable::new(),
            scopes: vec![main_scope],
            scope_index: 0,
        }
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope {
            instructions: Instructions(Vec::new()),
            last_instruction: None,
            previous_instruction: None,
        };

        self.scopes.push(scope);
        self.scope_index += 1;
        self.symbol_table = SymbolTable::new_enclosed(self.symbol_table.clone());
    }

    fn leave_scope(&mut self) -> Instructions {
        let removed_scope = self.scopes.pop();
        self.scope_index -= 1;

        self.symbol_table = *self.symbol_table.outer.as_ref().unwrap().clone();

        if let Some(scope) = removed_scope {
            scope.instructions
        } else {
            Instructions(Vec::new())
        }
    }

    fn current_instructions(&mut self) -> &mut Instructions {
        &mut self.scopes[self.scope_index].instructions
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
                instructions: self.current_instructions().clone(),
                constants: self.constants,
            },
            self.symbol_table,
        )
    }

    fn compile_statement(&mut self, s: Statement) {
        match s {
            Statement::Return(exp) => {
                self.compile_expression(*exp);
                self.emit_instruction(OP::RET_VAL, &[]);
            }
            Statement::Let(token, exp) => {
                self.compile_expression(*exp);

                match token.t_value.unwrap() {
                    TokenValue::Literal(name) => {
                        let (symbol_index, symbol_scope) = self.symbol_table.define(name);
                        match symbol_scope {
                            SymbolScope::Global => {
                                self.emit_instruction(OP::SET_GLOB, &[symbol_index as i32]);
                            }
                            SymbolScope::Local => {
                                self.emit_instruction(OP::SET_LOC, &[symbol_index as i32]);
                            }
                        }
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
            Expression::Call(func, args) => {
                self.compile_expression(*func);
                let args_len = args.len();
                for arg in args {
                    self.compile_expression(*arg);
                }

                self.emit_instruction(OP::CALL, &[args_len as i32]);
            }
            Expression::Function(identifiers, body) => {
                self.enter_scope();
                let args_len = identifiers.len();
                for ident in identifiers {
                    match ident.t_value.unwrap() {
                        TokenValue::Literal(name) => {
                            self.symbol_table.define(name);
                        }
                        _ => {}
                    }
                }

                self.compile_statement(*body);

                // If the function's last line is an expression, remove its pop
                // and replace it with a return value operation so that the
                // function returns the last expression's value. If there's no
                // pop, then make the function return null.
                if !self.remove_last_pop_if_present(Some(OP::RET_VAL)) {
                    self.emit_instruction(OP::RET_NONE, &[]);
                }

                let num_local_bindings = self.symbol_table.store.len();
                let instructions = self.leave_scope();
                let compiled_f = Object::CompiledFunction {
                    ins: instructions,
                    num_locals: num_local_bindings,
                    num_args: args_len,
                };
                let address = self.add_constant(compiled_f) as i32;
                self.emit_instruction(OP::CONSTANT, &[address]);
            }
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
                    if let Some((symbol_index, symbol_scope)) =
                        self.symbol_table.resolve(name.clone())
                    {
                        match symbol_scope {
                            SymbolScope::Global => {
                                self.emit_instruction(OP::GET_GLOB, &[symbol_index as i32]);
                            }
                            SymbolScope::Local => {
                                self.emit_instruction(OP::GET_LOC, &[symbol_index as i32]);
                            }
                        }
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
                self.remove_last_pop_if_present(None);

                let jump_offset = self.emit_instruction(OP::JMP, &[6666]);

                // TODO: merge code into a separate backfilling function
                match *alternative {
                    // If there is no else branch, we can fix the value of the
                    // conditional jump.
                    Statement::None => {
                        // As a single-pass compiler, we now need to backfill the
                        // correct address for the conditional jump.
                        let after_conseq_offset = self.current_instructions().0.len();
                        // Get the conditional jump op code.
                        let op = self.current_instructions().0[placeholder_offset];
                        // Create a fixed conditional jump instruction.
                        let new_cnd_jmp =
                            code::make(op, &[after_conseq_offset.try_into().unwrap()]);
                        // Replace the placeholder instruction.
                        self.current_instructions()
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
                        let after_conseq_offset = self.current_instructions().0.len();
                        // Get the conditional jump op code.
                        let op = self.current_instructions().0[placeholder_offset];
                        // Create a fixed conditional jump instruction.
                        let new_cnd_jmp =
                            code::make(op, &[after_conseq_offset.try_into().unwrap()]);
                        // Replace the placeholder instruction.
                        self.current_instructions()
                            .replace_at_offset(placeholder_offset, new_cnd_jmp);

                        self.compile_statement(*alternative);
                        self.remove_last_pop_if_present(None);
                    }
                }

                let after_alternative_offset = self.current_instructions().0.len();
                // Get the jump op code.
                let op = self.current_instructions().0[jump_offset];
                // Create a fixed conditional jump instruction.
                let new_jmp = code::make(op, &[after_alternative_offset.try_into().unwrap()]);
                // Replace the placeholder instruction.
                self.current_instructions()
                    .replace_at_offset(jump_offset, new_jmp);
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

        self.set_last_instruction(op, pos);

        pos
    }

    fn remove_last_pop_if_present(&mut self, replace: Option<OpCode>) -> bool {
        if let Some(ins) = &self.scopes[self.scope_index].last_instruction {
            if ins.op == OP::POP {
                self.scopes[self.scope_index].instructions.0.pop();
                self.scopes[self.scope_index].last_instruction =
                    self.scopes[self.scope_index].previous_instruction.clone();

                if let Some(replacement) = replace {
                    self.emit_instruction(replacement, &[]);
                }

                return true;
            }
        }

        false
    }

    fn set_last_instruction(&mut self, op: OpCode, pos: usize) {
        let previous = self.scopes[self.scope_index].last_instruction.clone();
        let last = Some(EmittedInstruction { op, pos });

        self.scopes[self.scope_index].previous_instruction = previous;
        self.scopes[self.scope_index].last_instruction = last;
    }

    // Returns the starting point of the added instruction
    fn add_instruction(&mut self, mut instructions: Instructions) -> usize {
        let pos = self.current_instructions().0.len();
        self.scopes[self.scope_index]
            .instructions
            .0
            .append(&mut instructions.0);

        pos
    }
}
