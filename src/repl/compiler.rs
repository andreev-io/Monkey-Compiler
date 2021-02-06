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
    Free,
    Global,
    Local,
    Function,
}

#[derive(Clone, Debug)]
struct Symbol {
    name: String,
    scope: SymbolScope,
    index: usize,
}

#[derive(Clone)]
pub struct SymbolTable {
    store: (HashMap<String, Symbol>, bool),
    outer: Option<Box<SymbolTable>>,
    free_symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            store: (HashMap::new(), false),
            outer: None,
            free_symbols: Vec::new(),
        }
    }

    pub fn new_enclosed(s: SymbolTable) -> SymbolTable {
        SymbolTable {
            store: (HashMap::new(), false),
            outer: Some(Box::new(s)),
            free_symbols: Vec::new(),
        }
    }

    fn define_function_name(&mut self, name: String) -> Symbol {
        let s = Symbol {
            name: name.clone(),
            scope: SymbolScope::Function,
            index: self.free_symbols.len(),
        };

        self.store.0.insert(name, s.clone());
        self.store.1 = true;

        s
    }

    fn define_free(&mut self, s: Symbol) -> Symbol {
        let n = s.name.clone();
        self.free_symbols.push(s);

        Symbol {
            name: n,
            scope: SymbolScope::Free,
            index: self.free_symbols.len() - 1,
        }
    }

    fn define(&mut self, name: String) -> Symbol {
        let scope = if let Some(_) = &self.outer {
            SymbolScope::Local
        } else {
            SymbolScope::Global
        };

        let index = if self.store.1 {
            self.store.0.len() - 1
        } else {
            self.store.0.len()
        };

        let symbol = Symbol {
            name: name.clone(),
            scope: scope.clone(),
            index: index,
        };

        self.store.0.insert(name, symbol.clone());
        symbol.clone()
    }

    fn resolve(&mut self, name: String) -> Option<Symbol> {
        if let Some(symbol) = self.store.0.get(&name) {
            Some(symbol.clone())
        } else {
            if let Some(outer) = &mut self.outer {
                if let Some(outer_symbol) = outer.resolve(name.clone()) {
                    match outer_symbol.scope {
                        SymbolScope::Global => return Some(outer_symbol),
                        _ => return Some(self.define_free(outer_symbol)),
                    }
                }
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
                // Define first, compile after. This way recursion works.
                match token.t_value.unwrap() {
                    TokenValue::Literal(name) => {
                        let symbol = self.symbol_table.define(name);
                        self.compile_expression(*exp);
                        match symbol.scope {
                            SymbolScope::Global => {
                                self.emit_instruction(OP::SET_GLOB, &[symbol.index as i32]);
                            }
                            SymbolScope::Local => {
                                self.emit_instruction(OP::SET_LOC, &[symbol.index as i32]);
                            }
                            _ => {}
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

    fn load_symbol(&mut self, s: &Symbol) {
        match s.scope {
            SymbolScope::Global => {
                self.emit_instruction(OP::GET_GLOB, &[s.index as i32]);
            }
            SymbolScope::Local => {
                self.emit_instruction(OP::GET_LOC, &[s.index as i32]);
            }
            SymbolScope::Free => {
                self.emit_instruction(OP::GET_FREE, &[s.index as i32]);
            }
            SymbolScope::Function => {
                self.emit_instruction(OP::CUR_CLOS, &[]);
            }
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
            Expression::Function(f_name, identifiers, body) => {
                self.enter_scope();

                if let Some(f_name) = f_name {
                    self.symbol_table.define_function_name(f_name);
                }

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

                let num_local_bindings = if self.symbol_table.store.1 {
                    self.symbol_table.store.0.len() - 1
                } else {
                    self.symbol_table.store.0.len()
                };
                let num_free_bindings = self.symbol_table.free_symbols.len();

                // The order of operations is important here. We just compiled
                // the function body in the inner scope. We now take a record of
                // the free symbols that the compiled function needs access to
                // and leave the scope. After leaving the scope, we load all the
                // necessary symbols onto the stack within current scope, so
                // they are available for the function in the inner scope.
                let mut free_symbols = self.symbol_table.free_symbols.clone();
                let instructions = self.leave_scope();
                while let Some(free_symbol) = free_symbols.pop() {
                    self.load_symbol(&free_symbol);
                }

                let compiled_f = Object::CompiledClosure {
                    ins: instructions,
                    num_locals: num_local_bindings,
                    num_args: args_len,
                    frees: Vec::new(),
                };

                let address = self.add_constant(compiled_f) as i32;
                self.emit_instruction(OP::CLOS, &[address, num_free_bindings as i32]);
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
                    if let Some(symbol) = self.symbol_table.resolve(name.clone()) {
                        self.load_symbol(&symbol);
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
