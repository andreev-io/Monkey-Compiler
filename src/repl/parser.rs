use crate::repl::{
    lexer::{Lexer, Token, TokenType, TokenValue},
    object::{Environment, Function, Node, Object},
};
use std::{io::Error, io::ErrorKind};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<Error>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer) -> Parser {
        let mut p = Parser {
            lexer: lexer,
            cur_token: Token::new(),
            peek_token: Token::new(),
            errors: Vec::new(),
        };

        p.next_token();
        p.next_token();

        p
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_prefix(&mut self) -> Option<Box<dyn Expression>> {
        match self.cur_token.t_type {
            TokenType::Ident => Some(Box::new(Identifier {
                token: self.cur_token.clone(),
            })),
            TokenType::Int => Some(Box::new(IntegerLiteral {
                token: self.cur_token.clone(),
            })),
            TokenType::Bang | TokenType::Minus => {
                let cur_token = self.cur_token.clone();
                self.next_token();

                let right_expression = self.parse_expression(Precedence::Prefix);
                Some(Box::new(PrefixExpression {
                    token: cur_token,
                    right: right_expression,
                }))
            }
            TokenType::False | TokenType::True => Some(Box::new(BooleanExpression {
                token: self.cur_token.clone(),
            })),
            TokenType::LParen => {
                self.next_token();
                let expression = self.parse_expression(Precedence::Lowest);
                match self.expect_peek(TokenType::RParen) {
                    Ok(()) => expression,
                    _ => None,
                }
            }
            TokenType::If => {
                let token = self.cur_token.clone();

                if self.expect_peek(TokenType::LParen).is_err() {
                    return None;
                }

                self.next_token();
                let condition = self.parse_expression(Precedence::Lowest);

                if self.expect_peek(TokenType::RParen).is_err() {
                    return None;
                }

                if self.expect_peek(TokenType::LBrace).is_err() {
                    return None;
                }

                let consequence = self.parse_block_statement();

                let alternative = if self.peek_token.t_type == TokenType::Else {
                    self.next_token();

                    if self.expect_peek(TokenType::LBrace).is_err() {
                        return None;
                    }

                    Some(self.parse_block_statement())
                } else {
                    None
                };

                Some(Box::new(IfExpression {
                    token,
                    condition,
                    consequence: Some(consequence),
                    alternative,
                }))
            }
            TokenType::Function => {
                let token = self.cur_token.clone();
                if self.expect_peek(TokenType::LParen).is_err() {
                    return None;
                }

                let parameters = self.parse_function_params();
                if self.expect_peek(TokenType::LBrace).is_err() {
                    return None;
                }

                let body = self.parse_block_statement();

                Some(Box::new(FunctionLiteral {
                    token,
                    parameters,
                    body,
                }))
            }
            _ => None,
        }
    }

    fn parse_function_params(&mut self) -> Vec<Box<Identifier>> {
        let mut v = Vec::new();

        if self.peek_token.t_type == TokenType::RParen {
            self.next_token();
            return v;
        }

        self.next_token();
        match self.cur_token.t_type {
            TokenType::Ident => {}
            _ => return Vec::new(),
        }

        let identifier = Identifier {
            token: self.cur_token.clone(),
        };

        v.push(Box::new(identifier));

        while self.peek_token.t_type == TokenType::Comma {
            self.next_token();
            self.next_token();

            match self.cur_token.t_type {
                TokenType::Ident => {}
                _ => return Vec::new(),
            }

            let identifier = Identifier {
                token: self.cur_token.clone(),
            };

            v.push(Box::new(identifier));
        }

        if self.expect_peek(TokenType::RParen).is_err() {
            return Vec::new();
        }

        v
    }

    fn parse_block_statement(&mut self) -> Box<BlockStatement> {
        let token = self.cur_token.clone();
        self.next_token();
        let mut statements = Vec::new();

        while self.cur_token.t_type != TokenType::RBrace && self.cur_token.t_type != TokenType::EOF
        {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }
            self.next_token();
        }

        Box::new(BlockStatement { token, statements })
    }

    fn get_precedence(token_type: TokenType) -> Precedence {
        match token_type {
            TokenType::Eq | TokenType::NotEq => Precedence::Equals,
            TokenType::LT | TokenType::GT => Precedence::LessGreater,
            TokenType::Plus | TokenType::Minus => Precedence::Sum,
            TokenType::Slash | TokenType::Asterisk => Precedence::Product,
            TokenType::LParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    fn parse_infix(&mut self, left: Box<dyn Expression>) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        match token.t_type {
            TokenType::LParen => {
                let arguments = self.parse_call_arguments();
                Some(Box::new(CallExpression {
                    token,
                    arguments,
                    function: left,
                }))
            }
            _ => {
                let precedence = Parser::get_precedence(self.cur_token.t_type.clone());

                self.next_token();
                let right = self.parse_expression(precedence);

                Some(Box::new(InfixExpression {
                    left: Some(left),
                    right,
                    token,
                }))
            }
        }
    }

    fn parse_call_arguments(&mut self) -> Vec<Box<dyn Expression>> {
        let mut v = Vec::new();
        if self.peek_token.t_type == TokenType::RParen {
            self.next_token();
            return v;
        }

        self.next_token();
        if let Some(exp) = self.parse_expression(Precedence::Lowest) {
            v.push(exp);
        };

        while self.peek_token.t_type == TokenType::Comma {
            self.next_token();
            self.next_token();

            if let Some(exp) = self.parse_expression(Precedence::Lowest) {
                v.push(exp);
            };
        }

        if self.expect_peek(TokenType::RParen).is_err() {
            return Vec::new();
        }

        v
    }

    // Match expectation by variant, not value. If matched, advance by a single
    // token.
    fn expect_peek(&mut self, token_type: TokenType) -> Result<(), Error> {
        if token_type == self.peek_token.t_type {
            self.next_token();
            return Ok(());
        } else {
            return Err(Error::new(
                ErrorKind::Other,
                format!(
                    "expected next token to be {:?}, got {:?}",
                    token_type, self.peek_token.t_type
                ),
            ));
        }
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token.t_type {
            TokenType::Let => match self.parse_let_statement() {
                Some(statement) => Some(Box::new(statement)),
                None => None,
            },
            TokenType::Return => match self.parse_return_statement() {
                Some(statement) => Some(Box::new(statement)),
                None => None,
            },
            _ => match self.parse_expression_statement() {
                Some(statement) => Some(Box::new(statement)),
                None => None,
            },
        }
    }

    fn parse_let_statement(&mut self) -> Option<impl Statement> {
        let token = self.cur_token.clone();
        match self.expect_peek(TokenType::Ident) {
            Err(e) => {
                self.errors.push(e);
                return None;
            }
            _ => {}
        }

        let name = Identifier {
            token: self.cur_token.clone(),
        };

        match self.expect_peek(TokenType::Assign) {
            Err(e) => {
                self.errors.push(e);
                return None;
            }
            _ => {}
        }

        self.next_token();
        let value = self.parse_expression(Precedence::Lowest);
        if self.peek_token.t_type == TokenType::Semicolon {
            self.next_token();
        }

        Some(LetStatement { token, name, value })
    }

    fn parse_expression_statement(&mut self) -> Option<impl Statement> {
        let token = self.cur_token.clone();
        let expression = self.parse_expression(Precedence::Lowest);
        if self.peek_token.t_type == TokenType::Semicolon {
            self.next_token();
        }

        Some(ExpressionStatement {
            token: token,
            value: expression,
        })
    }

    // This and the callee methods are the core idea of Pratt parsing. The
    // ultimate goal is to have higher precedence operations deeper in the tree
    // than the lower precedence operations.
    //
    // Two parameters matter: the precedence argument represents the
    // right-binding power, the power of the expression to bind the expressions
    // to the right of it to itself. The left-binding power comes from peeked
    // precedence. Consider 2 + 3 * 4. 3 here is an expression. The left-binding
    // power of * is higher than the right-binding power of +, so * consumes 3
    // as its left child node. Hence the name top-down operator precedence.
    //
    // The rest of the idea is that the same token can be treated differently in
    // prefix & infix contexts. Consider, say 2 - -3 * 4. Here -3 is parsed as
    // the left child node. Notice how prefixes are defined to have high
    // precedence.
    fn parse_expression(&mut self, p: Precedence) -> Option<Box<dyn Expression>> {
        let mut left = self.parse_prefix();

        while self.peek_token.t_type != TokenType::Semicolon
            && p < Parser::get_precedence(self.peek_token.t_type.clone())
        {
            self.next_token();

            if let Some(inner_left) = left {
                left = self.parse_infix(inner_left);
            } else {
                return left;
            }
        }

        left
    }

    fn parse_return_statement(&mut self) -> Option<impl Statement> {
        let token = self.cur_token.clone();
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest);
        if self.peek_token.t_type == TokenType::Semicolon {
            self.next_token();
        }

        Some(ReturnStatement { token, value })
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Box<dyn Statement>> = Vec::new();
        let mut t = &self.cur_token;
        while t.t_type != TokenType::EOF {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }

            self.next_token();
            t = &self.cur_token;
        }

        Program { statements }
    }
}

#[derive(PartialEq, Eq, PartialOrd)]
enum Precedence {
    Lowest = 1,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub trait Statement: Node {
    fn get_token(&self) -> Token;
    fn get_expression(&self) -> &Option<Box<dyn Expression>>;
    fn string(&self) -> String;
}

pub struct BlockStatement {
    // the { token
    token: Token,
    statements: Vec<Box<dyn Statement>>,
}

impl Statement for BlockStatement {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    // technically this should be the last statement/expression of the block
    // statement
    fn get_expression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn string(&self) -> String {
        let mut s: String = String::from("{ ");

        let mut v = Vec::new();
        for statement in &self.statements {
            v.push(statement.string());
        }

        s.push_str(&v.join("\n"));
        s.push_str(" }");

        s
    }
}

impl Node for BlockStatement {
    fn eval(self: Box<Self>, env: &mut Box<Environment>) -> Box<Object> {
        let mut result = Box::new(Object::Null);
        for statement in self.statements {
            result = statement.eval(env);

            match *result {
                Object::ReturnValue(_) => {
                    return result;
                }
                _ => {}
            }
        }

        result
    }
}

struct LetStatement {
    // token is the let token
    token: Token,

    // value is the RHS expression
    value: Option<Box<dyn Expression>>,

    // name is the LHS identiier
    name: Identifier,
}

impl Statement for LetStatement {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_expression(&self) -> &Option<Box<dyn Expression>> {
        &self.value
    }

    fn string(&self) -> String {
        let mut s: String = String::from("");
        s.push_str(&self.token.string());
        s.push_str(" ");
        s.push_str(&self.name.string());
        s.push_str(" = ");

        if let Some(exp) = &self.value {
            s.push_str(&exp.string());
        }

        s.push_str(";");

        s
    }
}

impl Node for LetStatement {
    fn eval(self: Box<Self>, env: &mut Box<Environment>) -> Box<Object> {
        let val = if let Some(val) = self.value {
            val.eval(env)
        } else {
            Box::new(Object::Null)
        };

        match &self.name.token.t_value {
            Some(TokenValue::Literal(name)) => {
                env.set(name.to_string(), val);
            }
            _ => {}
        };

        Box::new(Object::Null)
    }
}

struct ReturnStatement {
    // token is the return token
    token: Token,
    // value is the RHS expression
    value: Option<Box<dyn Expression>>,
}

impl Statement for ReturnStatement {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_expression(&self) -> &Option<Box<dyn Expression>> {
        &self.value
    }

    fn string(&self) -> String {
        let mut s: String = String::from("");

        s.push_str(&self.token.string());
        s.push_str(" ");

        if let Some(exp) = &self.value {
            s.push_str(&exp.string());
        }

        s.push_str(";");

        s
    }
}

impl Node for ReturnStatement {
    fn eval(self: Box<Self>, env: &mut Box<Environment>) -> Box<Object> {
        if let Some(v) = self.value {
            Box::new(Object::ReturnValue(v.eval(env)))
        } else {
            Box::new(Object::ReturnValue(Box::new(Object::Null)))
        }
    }
}

// One-liners that don't do much, say x+5;
struct ExpressionStatement {
    // first token of the expression
    token: Token,
    // value of the expression
    value: Option<Box<dyn Expression>>,
}

impl Statement for ExpressionStatement {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_expression(&self) -> &Option<Box<dyn Expression>> {
        &self.value
    }

    fn string(&self) -> String {
        let mut s: String = String::from("");

        if let Some(exp) = &self.value {
            s.push_str(&exp.string());
        }

        s
    }
}

impl Node for ExpressionStatement {
    fn eval(self: Box<Self>, env: &mut Box<Environment>) -> Box<Object> {
        match self.value {
            Some(exp) => exp.eval(env),
            _ => Box::new(Object::Null),
        }
    }
}

pub trait Expression: Node {
    fn get_token(&self) -> Token;
    fn get_left_subexpression(&self) -> &Option<Box<dyn Expression>>;
    fn get_right_subexpression(&self) -> &Option<Box<dyn Expression>>;
    fn string(&self) -> String;
}

struct CallExpression {
    // the ( token
    token: Token,
    function: Box<dyn Expression>,
    arguments: Vec<Box<dyn Expression>>,
}

impl Expression for CallExpression {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_left_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn get_right_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn string(&self) -> String {
        let mut s: String = String::from("");

        let mut v = Vec::new();
        for arg in &self.arguments {
            v.push(arg.string());
        }

        s.push_str(&self.function.string());

        s.push_str("(");
        s.push_str(&v.join(", "));
        s.push_str(");");

        s
    }
}

impl Node for CallExpression {
    fn eval(self: Box<Self>, env: &mut Box<Environment>) -> Box<Object> {
        self.function.eval(env)
    }
}

struct InfixExpression {
    // in 5+5, token is +
    token: Token,
    // LHS expression
    left: Option<Box<dyn Expression>>,
    // RHS expression
    right: Option<Box<dyn Expression>>,
}

impl Expression for InfixExpression {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_left_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &self.left
    }

    fn get_right_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &self.right
    }

    fn string(&self) -> String {
        let mut s: String = String::from("");

        s.push_str("(");
        if let Some(exp) = &self.left {
            s.push_str(&exp.string());
        }

        s.push_str(&format!(" {} ", self.token.string()));

        if let Some(exp) = &self.right {
            s.push_str(&exp.string());
        }

        s.push_str(")");

        s
    }
}

impl Node for InfixExpression {
    fn eval(self: Box<Self>, env: &mut Box<Environment>) -> Box<Object> {
        let (left, right) = match (self.left, self.right) {
            (Some(l), Some(r)) => (l.eval(env), r.eval(env)),
            (Some(l), _) => (l.eval(env), Box::new(Object::Null)),
            (_, Some(r)) => (Box::new(Object::Null), r.eval(env)),
            (_, _) => (Box::new(Object::Null), Box::new(Object::Null)),
        };

        match (*left, &self.token.t_type, *right) {
            (Object::Integer(l), TokenType::Plus, Object::Integer(r)) => {
                Box::new(Object::Integer(l + r))
            }
            (Object::Integer(l), TokenType::Minus, Object::Integer(r)) => {
                Box::new(Object::Integer(l - r))
            }
            (Object::Integer(l), TokenType::Asterisk, Object::Integer(r)) => {
                Box::new(Object::Integer(l * r))
            }
            (Object::Integer(l), TokenType::Slash, Object::Integer(r)) => {
                Box::new(Object::Integer(l / r))
            }
            (Object::Integer(l), TokenType::LT, Object::Integer(r)) => {
                Box::new(Object::Boolean(l < r))
            }
            (Object::Integer(l), TokenType::GT, Object::Integer(r)) => {
                Box::new(Object::Boolean(l > r))
            }
            (Object::Integer(l), TokenType::Eq, Object::Integer(r)) => {
                Box::new(Object::Boolean(l == r))
            }
            (Object::Integer(l), TokenType::NotEq, Object::Integer(r)) => {
                Box::new(Object::Boolean(l != r))
            }
            (Object::Boolean(l), TokenType::Eq, Object::Boolean(r)) => {
                Box::new(Object::Boolean(l == r))
            }
            (Object::Boolean(l), TokenType::NotEq, Object::Boolean(r)) => {
                Box::new(Object::Boolean(l != r))
            }
            (_, _, _) => Box::new(Object::Null),
        }
    }
}

struct PrefixExpression {
    // in !true, token is !
    token: Token,
    // RHS expression
    right: Option<Box<dyn Expression>>,
}

impl Expression for PrefixExpression {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_left_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn get_right_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &self.right
    }

    fn string(&self) -> String {
        let mut s: String = String::from("");

        s.push_str("(");
        s.push_str(&self.token.string());
        if let Some(exp) = &self.right {
            s.push_str(&exp.string());
        }

        s.push_str(")");

        s
    }
}

impl Node for PrefixExpression {
    fn eval(self: Box<Self>, env: &mut Box<Environment>) -> Box<Object> {
        match self.right {
            Some(exp) => match (*exp.eval(env), &self.token.t_type) {
                (Object::Boolean(b), TokenType::Bang) => Box::new(Object::Boolean(!b)),
                (Object::Null, TokenType::Bang) => Box::new(Object::Boolean(true)),
                (_, TokenType::Bang) => Box::new(Object::Boolean(false)),
                (Object::Integer(i), TokenType::Minus) => Box::new(Object::Integer(-1 * i)),
                (_, _) => Box::new(Object::Null),
            },
            _ => Box::new(Object::Null),
        }
    }
}

// boolean expression
struct BooleanExpression {
    token: Token,
}

impl Expression for BooleanExpression {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_left_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn get_right_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn string(&self) -> String {
        self.token.string()
    }
}

impl Node for BooleanExpression {
    fn eval(self: Box<Self>, _env: &mut Box<Environment>) -> Box<Object> {
        match self.token.t_type {
            TokenType::False => Box::new(Object::Boolean(false)),
            TokenType::True => Box::new(Object::Boolean(true)),
            _ => Box::new(Object::Null),
        }
    }
}

// simplest expression
#[derive(Clone)]
pub struct Identifier {
    token: Token,
}

impl Expression for Identifier {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_left_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn get_right_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn string(&self) -> String {
        self.token.string()
    }
}

impl Node for Identifier {
    fn eval(self: Box<Self>, env: &mut Box<Environment>) -> Box<Object> {
        let ret = Box::new(Object::Null);
        match &self.token.t_value {
            Some(TokenValue::Literal(name)) => env.get(name.to_string()),
            _ => ret,
        }
    }
}

struct IfExpression {
    // if token
    token: Token,
    condition: Option<Box<dyn Expression>>,
    consequence: Option<Box<BlockStatement>>,
    alternative: Option<Box<BlockStatement>>,
}

impl Expression for IfExpression {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_left_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn get_right_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn string(&self) -> String {
        let mut s: String = String::from("");

        s.push_str("if");
        if let Some(condition) = &self.condition {
            s.push_str(" ");
            s.push_str(&condition.string());
        }

        s.push_str(" ");
        if let Some(consequence) = &self.consequence {
            s.push_str(&consequence.string());
        }

        s.push_str(" ");
        if let Some(alternative) = &self.alternative {
            s.push_str("else ");
            s.push_str(&alternative.string());
        }

        s
    }
}

impl Node for IfExpression {
    fn eval(self: Box<Self>, env: &mut Box<Environment>) -> Box<Object> {
        let evaled_cond = if let Some(cond) = self.condition {
            cond.eval(env)
        } else {
            Box::new(Object::Null)
        };

        let branch = match *evaled_cond {
            Object::Null | Object::Boolean(false) => false,
            _ => true,
        };

        if branch {
            if let Some(consequence) = self.consequence {
                consequence.eval(env)
            } else {
                Box::new(Object::Null)
            }
        } else {
            if let Some(alternative) = self.alternative {
                alternative.eval(env)
            } else {
                Box::new(Object::Null)
            }
        }
    }
}

struct FunctionLiteral {
    // the fn token
    token: Token,
    parameters: Vec<Box<Identifier>>,
    body: Box<BlockStatement>,
}

impl Expression for FunctionLiteral {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_left_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn get_right_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn string(&self) -> String {
        let mut s: String = String::from("");

        let mut v = Vec::new();
        for param in &self.parameters {
            v.push(param.string());
        }

        s.push_str(&self.token.string());
        s.push_str("(");
        s.push_str(&v.join(", "));
        s.push_str(") ");
        s.push_str(&self.body.string());

        s
    }
}

impl Node for FunctionLiteral {
    fn eval(self: Box<Self>, _env: &mut Box<Environment>) -> Box<Object> {
        Box::new(Object::Function(Box::new(Function::new(
            self.parameters.clone(),
            self.body,
        ))))
    }
}

struct IntegerLiteral {
    token: Token,
}

impl Expression for IntegerLiteral {
    fn get_token(&self) -> Token {
        self.token.clone()
    }

    fn get_left_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn get_right_subexpression(&self) -> &Option<Box<dyn Expression>> {
        &None
    }

    fn string(&self) -> String {
        self.token.string()
    }
}

impl Node for IntegerLiteral {
    fn eval(self: Box<Self>, _env: &mut Box<Environment>) -> Box<Object> {
        match self.token.t_value {
            Some(TokenValue::Numeric(value)) => Box::new(Object::Integer(value)),
            _ => Box::new(Object::Null),
        }
    }
}

pub struct Program {
    statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn eval(self: Box<Self>, env: &mut Box<Environment>) -> Box<Object> {
        let mut result = Box::new(Object::Null);
        for statement in self.statements {
            result = statement.eval(env);

            match *result {
                Object::ReturnValue(_) => {
                    return result;
                }
                _ => {}
            }
        }

        result
    }
}

impl Program {
    #[allow(dead_code)]
    pub fn string(&self) -> String {
        let mut s: String = String::from("");
        for statement in &self.statements {
            s.push_str(&statement.string());
        }

        s
    }
}

#[cfg(test)]
mod test {
    use crate::repl::{
        lexer::{Lexer, Token, TokenType, TokenValue},
        parser::Parser,
    };

    #[test]
    fn test_function_literal() {
        let input = r#"fn(x, y) {   x+ y }"#.chars().collect();
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let statements = program.statements;
        assert_eq!(statements.len(), 1);
        if let Some(exp) = statements[0].get_expression() {
            assert_eq!(TokenType::Function, exp.get_token().t_type);
            assert_eq!("fn(x, y) { (x + y) }", exp.string());
        } else {
            panic!("missing inner expression");
        }

        let input = r#"fn() {   x+ y }"#.chars().collect();
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let statements = program.statements;
        assert_eq!(statements.len(), 1);
        if let Some(exp) = statements[0].get_expression() {
            assert_eq!(TokenType::Function, exp.get_token().t_type);
            assert_eq!("fn() { (x + y) }", exp.string());
        } else {
            panic!("missing inner expression");
        }
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2*3, 4+  5)".chars().collect();
        let l = Lexer::new(&input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();

        assert_eq!("add(1, (2 * 3), (4 + 5));", program.string());
        assert_eq!(program.statements.len(), 1);
        if let Some(exp) = program.statements[0].get_expression() {
            assert_eq!("add(1, (2 * 3), (4 + 5));", exp.string());
        } else {
            panic!("missing inner expression");
        }
    }

    #[test]
    fn test_return_statements() {
        let input = r#"return 5;
    return 10;
    return 838383"#
            .chars()
            .collect();

        let l = Lexer::new(&input);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        assert_eq!(program.statements.len(), 3);
        assert_eq!(
            program.statements[0].get_token(),
            Token {
                t_value: None,
                t_type: TokenType::Return
            }
        );
        assert_eq!(
            program.statements[1].get_token(),
            Token {
                t_value: None,
                t_type: TokenType::Return
            }
        );
        assert_eq!(
            program.statements[2].get_token(),
            Token {
                t_value: None,
                t_type: TokenType::Return
            }
        );

        assert_eq!("return 5;", program.statements[0].string());
        assert_eq!("return 10;", program.statements[1].string());
        assert_eq!("return 838383;", program.statements[2].string());
    }

    #[test]
    fn test_let_statements() {
        let input = r#"let x = 5;
    let y = 10;
    let foobar = 838383"#
            .chars()
            .collect();

        let l = Lexer::new(&input);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        assert_eq!(program.statements.len(), 3);
        assert_eq!(
            program.statements[0].get_token(),
            Token {
                t_value: None,
                t_type: TokenType::Let
            }
        );
        assert_eq!(
            program.statements[1].get_token(),
            Token {
                t_value: None,
                t_type: TokenType::Let
            }
        );
        assert_eq!(
            program.statements[2].get_token(),
            Token {
                t_value: None,
                t_type: TokenType::Let
            }
        );

        assert_eq!("let x = 5;", program.statements[0].string());
        assert_eq!("let y = 10;", program.statements[1].string());
        assert_eq!("let foobar = 838383;", program.statements[2].string());
    }

    #[test]
    fn test_identifier_expression() {
        let input = String::from("foobar;");
        let c = input.chars().collect();

        let l = Lexer::new(&c);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        assert_eq!(program.statements.len(), 1);
        let token = program.statements[0].get_token();

        if let Some(val) = token.t_value {
            assert!(token.t_type == TokenType::Ident);
            assert!(val == TokenValue::Literal(String::from("foobar")));
        } else {
            panic!("empty token")
        }
    }

    #[test]
    fn test_boolean_literal_expression() {
        let input = String::from("false;");
        let c = input.chars().collect();

        let l = Lexer::new(&c);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        assert_eq!(program.statements.len(), 1);
        let token = program.statements[0].get_token();

        assert!(token.t_type == TokenType::False);
        assert_eq!(
            program.statements[0]
                .get_expression()
                .as_ref()
                .unwrap()
                .string(),
            String::from("false")
        );

        let input = String::from("false == false");
        let c = input.chars().collect();

        let l = Lexer::new(&c);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        assert_eq!(program.statements.len(), 1);
        if let Some(exp) = program.statements[0].get_expression() {
            assert_eq!(TokenType::Eq, exp.get_token().t_type);
            assert_eq!("(false == false)", exp.string());
            assert_eq!(
                "false",
                exp.get_left_subexpression().as_ref().unwrap().string()
            );
            assert_eq!(
                "false",
                exp.get_right_subexpression().as_ref().unwrap().string()
            );
        } else {
            panic!("no inner expression");
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = String::from("5;");
        let c = input.chars().collect();

        let l = Lexer::new(&c);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        assert_eq!(program.statements.len(), 1);
        let token = program.statements[0].get_token();

        if let Some(val) = token.t_value {
            assert!(token.t_type == TokenType::Int);
            assert!(val == TokenValue::Numeric(5));
        } else {
            panic!("empty token")
        }
    }

    #[test]
    fn test_prefix_expression() {
        // !5; is an expression statement and also a prefix expression
        let input = String::from("!5;");
        let c = input.chars().collect();

        let l = Lexer::new(&c);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        assert_eq!(program.statements.len(), 1);
        let token = program.statements[0].get_token();
        assert_eq!(token.t_type, TokenType::Bang);
        let inner_expression = program.statements[0].get_expression();
        if let Some(exp) = inner_expression {
            let right = exp.get_right_subexpression();
            if let Some(right) = right {
                assert_eq!(right.get_token().t_type, TokenType::Int);
                assert_eq!(right.get_token().t_value, Some(TokenValue::Numeric(5)));
            } else {
                panic!("no right subexpression");
            }
        } else {
            panic!("no inner expression");
        }

        let input = String::from("-15;");
        let c = input.chars().collect();

        let l = Lexer::new(&c);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        assert_eq!(program.statements.len(), 1);
        let token = program.statements[0].get_token();
        assert_eq!(token.t_type, TokenType::Minus);
        let inner_expression = program.statements[0].get_expression();
        if let Some(exp) = inner_expression {
            let right = exp.get_right_subexpression();
            if let Some(right) = right {
                assert_eq!(right.get_token().t_type, TokenType::Int);
                assert_eq!(right.get_token().t_value, Some(TokenValue::Numeric(15)));
            } else {
                panic!("no right subexpression");
            }
        } else {
            panic!("no inner expression");
        }
    }

    #[test]
    fn test_infix_expression() {
        struct InfixTest {
            input: String,
            left_int: i32,
            right_int: i32,
            operator: Token,
        }

        let v = vec![
            InfixTest {
                input: String::from("5+5;"),
                left_int: 5,
                right_int: 5,
                operator: Token {
                    t_type: TokenType::Plus,
                    t_value: None,
                },
            },
            InfixTest {
                input: String::from(" 5 < 6"),
                left_int: 5,
                right_int: 6,
                operator: Token {
                    t_type: TokenType::LT,
                    t_value: None,
                },
            },
            InfixTest {
                input: String::from("5 != 7  ;"),
                left_int: 5,
                right_int: 7,
                operator: Token {
                    t_type: TokenType::NotEq,
                    t_value: None,
                },
            },
            InfixTest {
                input: String::from("5 / 1"),
                left_int: 5,
                right_int: 1,
                operator: Token {
                    t_type: TokenType::Slash,
                    t_value: None,
                },
            },
        ];

        for t in v.iter() {
            let input = t.input.chars().collect();
            let l = Lexer::new(&input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            assert_eq!(program.statements.len(), 1);
            let exp = program.statements[0].get_expression();
            if let Some(exp) = exp {
                match (exp.get_left_subexpression(), exp.get_right_subexpression()) {
                    (Some(e_left), Some(e_right)) => {
                        assert_eq!(
                            e_left.get_token().t_value,
                            Some(TokenValue::Numeric(t.left_int))
                        );
                        assert_eq!(
                            e_right.get_token().t_value,
                            Some(TokenValue::Numeric(t.right_int))
                        );
                    }
                    _ => panic!("inner expression missing"),
                }
                assert_eq!(exp.get_token(), t.operator);
            } else {
                panic!("no inner expression");
            }
        }
    }

    #[test]
    fn test_operator_precedence() {
        struct PrecedenceTest {
            input: Vec<char>,
            output: String,
        }

        let v = vec![
            PrecedenceTest {
                input: "-a * b".chars().collect(),
                output: String::from("((-a) * b)"),
            },
            PrecedenceTest {
                input: "a + b * c + d / e - f".chars().collect(),
                output: String::from("(((a + (b * c)) + (d / e)) - f)"),
            },
            PrecedenceTest {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5".chars().collect(),
                output: String::from("((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            },
            PrecedenceTest {
                input: "2 - -3 * 4".chars().collect(),
                output: String::from("(2 - ((-3) * 4))"),
            },
            PrecedenceTest {
                input: "2 - 3 < 0 == true".chars().collect(),
                output: String::from("(((2 - 3) < 0) == true)"),
            },
            PrecedenceTest {
                input: "!(true == true)".chars().collect(),
                output: String::from("(!(true == true))"),
            },
            PrecedenceTest {
                input: "1 + (2 + 3) + 4".chars().collect(),
                output: String::from("((1 + (2 + 3)) + 4)"),
            },
        ];

        for test in v.iter() {
            let l = Lexer::new(&test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            assert_eq!(test.output, program.string());
        }
    }

    #[test]
    fn test_if_expression() {
        let input = String::from("if (x < y) { let x = 5; x } else { let x = 6; x");
        let c = input.chars().collect();

        let l = Lexer::new(&c);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        assert_eq!(program.statements.len(), 1);
        let token = program.statements[0].get_token();
        assert_eq!(token.t_type, TokenType::If);
        if let Some(exp) = program.statements[0].get_expression() {
            assert_eq!(
                "if (x < y) { let x = 5;\nx } else { let x = 6;\nx }",
                exp.string()
            );
        } else {
            panic!("no inner expression");
        }
    }
}
