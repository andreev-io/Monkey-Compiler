use crate::repl::parser::{BlockStatement, Expression, Identifier, Statement};
use std::collections::HashMap;

pub trait Node {
    fn eval<'s, 'a>(&'s self, env: &'a mut Box<Environment<'a>>) -> Box<Object<'a>>;
}

pub enum Object<'a> {
    Integer(i32),
    Boolean(bool),
    Function(Box<Function<'a>>),
    ReturnValue(Box<Object<'a>>),
    Null,
}

impl<'a> Object<'a> {
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

pub struct Environment<'a>(HashMap<String, Box<Object<'a>>>);

impl<'a> Environment<'a> {
    pub fn new() -> Environment<'a> {
        Environment(HashMap::new())
    }

    pub fn set(&mut self, name: String, val: Box<Object<'a>>) {
        self.0.insert(name, val);
    }

    pub fn get(&mut self, name: String) -> Box<Object> {
        self.0.remove(&name).unwrap()
    }
}

pub struct Function<'a> {
    parameters: Vec<Box<Identifier>>,
    body: &'a Box<BlockStatement>,
    env: Box<Environment<'a>>,
}

impl<'a> Function<'a> {
    pub fn new<'b: 'a>(parameters: Vec<Box<Identifier>>, body: &'b Box<BlockStatement>) -> Function<'a> {
        Function::<'a> {
            parameters,
            body,
            env: Box::new(Environment::new()),
        }
    }
}
