use crate::repl::eval::Node;
use crate::repl::parser::{BlockStatement, Expression, Identifier, Statement};
use std::collections::HashMap;

pub enum Object {
    Integer(i32),
    Boolean(bool),
    String(String),
    Function(Box<Function>),
    ReturnValue(Box<Object>),
    Array(Vec<Box<Object>>),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => i.to_string(),
            Object::Boolean(b) => b.to_string(),
            Object::ReturnValue(rv) => rv.inspect(),
            Object::String(s) => s.to_string(),
            Object::Array(arr) => {
                let mut s: String = String::from("[");
                let mut v = Vec::new();

                for el in arr.into_iter() {
                    v.push(el.inspect());
                }

                s.push_str(&v.join(","));
                s.push_str("]");

                s
            }
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

// Since our implementation is a naive one written in Rust, it inherits some of
// Rust's peculiarities. As such, every identifier "owns" the underlying value.
// That means that once an identifier is used, its underlying value is now owned
// by whatever consumed it. Further, since functions are themselves objects in
// our system, it means that any function can only be run once.
//
// On the plus side, garbage collection is unnecessary. On the other hand, we
// built a language whose memory model conforms to that of Rust, but doesn't
// have all the features of Rust.
//
// In other words, where Rust has ownership, borrowing, and lifetimes, our
// language only has ownership.
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

        let iter = args.into_iter().zip(self.parameters);
        for (arg_proto, param_proto) in iter {
            let arg = arg_proto.eval(env);
            let param = param_proto.string();
            new_env.set(param, arg);
        }

        self.body.eval(&mut new_env)
    }
}
