use crate::repl::parser::{BlockStatement, Expression, Identifier, Statement};
use std::collections::HashMap;

pub trait Node {
    fn eval(self: Box<Self>, env: &mut Box<Environment>) -> Box<Object>;
}

pub enum Object {
    Integer(i32),
    Boolean(bool),
    Function(Box<Function>),
    ReturnValue(Box<Object>),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => i.to_string(),
            Object::Boolean(b) => b.to_string(),
            Object::ReturnValue(rv) => rv.inspect(),
            Object::Function(f) => {
                let mut s: String = String::from("");

                let mut v = Vec::new();
                for param in &f.parameters {
                    v.push(param.string());
                }

                s.push_str("fn(");
                s.push_str(&v.join(", "));
                s.push_str(") {\n");
                s.push_str(&f.body.string());
                s.push_str("\n");

                s
            }
            Object::Null => String::from("null"),
        }
    }
}

pub struct Environment(HashMap<String, Box<Object>>);

impl Environment {
    pub fn new() -> Environment {
        Environment(HashMap::new())
    }

    pub fn set(&mut self, name: String, val: Box<Object>) {
        self.0.insert(name, val);
    }

    pub fn get(&mut self, name: String) -> Box<Object> {
        self.0.remove(&name).unwrap()
    }
}

pub struct Function {
    parameters: Vec<Box<Identifier>>,
    body: Box<BlockStatement>,
}

impl Function {
    pub fn new(parameters: Vec<Box<Identifier>>, body: Box<BlockStatement>) -> Function {
        Function { parameters, body }
    }
}
