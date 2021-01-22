use crate::repl::eval::Node;
use crate::repl::parser::{BlockStatement, Expression, Identifier, Statement};
use std::collections::HashMap;

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
                s.push_str(") ");
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

    pub fn get(&mut self, name: String) -> Option<Box<Object>> {
        self.0.remove(&name)
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

    pub fn eval_func(
        self,
        args: Vec<Box<dyn Expression>>,
        env: &mut Box<Environment>,
    ) -> Box<Object> {
        let mut new_env = Box::new(Environment::new());

        let iter = args.into_iter().zip(&self.parameters);
        for val in iter {
            let arg = val.0.eval(env);
            let param = val.1.string();
            new_env.set(param, arg);
        }

        self.body.eval(&mut new_env)
    }
}
