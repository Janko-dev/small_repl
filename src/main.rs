use std::{io::{self, Write}, str::{Chars}, iter::Peekable, slice::Iter};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Tokens {
    Plus,
    Minus,
    Star,
    Slash,
    Number,
    OpenParen,
    CloseParen,
}

#[derive(Debug, Clone)]
struct Token {
    lexeme: String,
    token_type: Tokens,
}

#[derive(Debug)]
enum Expr {
    Binary(Box<Option<Expr>>, Token, Box<Option<Expr>>),
    Unary(Token, Box<Option<Expr>>),
    Group(Box<Option<Expr>>),
    Primary(f64),
}

fn number(list: &mut Vec<Token>, chars: &mut Peekable<Chars>){
    let mut lexeme = String::new();

    while let Some(c @ '0'..='9') = chars.peek() {
        lexeme.push(*c);
        chars.next();   
    }

    if let Some(dot @ '.') = chars.peek() {
        lexeme.push(*dot);
        chars.next();
        while let Some(c @ '0'..='9') = chars.peek() {
            lexeme.push(*c);
            chars.next();
        }    
    }

    list.push(Token {lexeme, token_type: Tokens::Number});
}

fn add_operator(list: &mut Vec<Token>, chars: &mut Peekable<Chars>, token_type: Tokens){
    let mut lexeme = String::new();
    if let Some(c) = chars.next(){
        lexeme.push(c);
    }
    list.push(Token {lexeme, token_type});
}

fn lexer(list: &mut Vec<Token>, input: &str){
    let mut chars = input.chars().peekable();
    
    loop {
        match chars.peek() {
            Some('0'..='9') => number(list, &mut chars),
            Some('+') => add_operator(list, &mut chars, Tokens::Plus),
            Some('-') => add_operator(list, &mut chars, Tokens::Minus),
            Some('*') => add_operator(list, &mut chars, Tokens::Star),
            Some('/') => add_operator(list, &mut chars, Tokens::Slash),
            Some('(') => add_operator(list, &mut chars, Tokens::OpenParen),
            Some(')') => add_operator(list, &mut chars, Tokens::CloseParen),
            Some(' ' | '\t' | '\n') => {
                chars.next();
            },
            Some(c) => {
                println!("Unknown character: {}", c);
                chars.next();
            },
            None => {
                break;
            }
        }
    }
}

fn parse(list: &mut Vec<Token>) -> Option<Expr> {
    term(&mut list.iter().peekable())
}

fn term(list: &mut Peekable<Iter<Token>>) -> Option<Expr> {
    let mut left = factor(list);

    loop {
        match list.peek() {
            Some(Token { lexeme: _, token_type: Tokens::Plus }) |
            Some(Token { lexeme: _, token_type: Tokens::Minus }) => {
                let op = list.next().unwrap().clone();
                let right = factor(list);
                left = Some(Expr::Binary(Box::new(left), op, Box::new(right)));
            },
            _ => break,
        }
    };
    left
}

fn factor(list: &mut Peekable<Iter<Token>>) -> Option<Expr> {
    let mut left = unary(list);

    loop {
        match list.peek() {
            Some(Token { lexeme: _ , token_type: Tokens::Star }) |
            Some(Token { lexeme: _, token_type: Tokens::Slash }) => {
                let op = list.next().unwrap().clone();
                let right = unary(list);
                left = Some(Expr::Binary(Box::new(left), op, Box::new(right)));
            },
            _ => break,
        }
    };
    left
}

fn unary(list: &mut Peekable<Iter<Token>>) -> Option<Expr> {
    loop {
        match list.peek() {
            Some(Token { lexeme: _, token_type: Tokens::Minus }) => {
                let op = list.next().unwrap().clone();
                return Some(Expr::Unary(op, Box::new(unary(list))));
            },
            _ => break,
        }
    };
    return primary(list);
}

fn primary(list: &mut Peekable<Iter<Token>>) -> Option<Expr> {
    match list.peek() {
        Some(Token { lexeme , token_type: Tokens::Number }) => {
            list.next();
            let value: f64 = lexeme.parse().expect("failed to parse number");
            Some(Expr::Primary(value))
        },
        Some(Token { lexeme: _, token_type: Tokens::OpenParen }) => {
            list.next();
            let res = Expr::Group(Box::new(term(list)));
            
            if let Some(Token { lexeme: _, token_type: Tokens::CloseParen }) = list.next() {
                return Some(res);
            } else {
                println!("Missing closing parenthesis");
                None
            }
        },
        _ => {
            println!("Unknown primary symbol");
            None
        }
    }
}

fn eval(expr: Option<Expr>) -> Result<f64, String>{
    match expr {
        Some(Expr::Binary(l, op, r)) => {
            let left = eval(*l);
            if let Err(s) = left {
                return Err(s);
            }
            let right = eval(*r);
            if let Err(s) = right {
                return Err(s);
            }
            match op.token_type {
                Tokens::Plus => return Ok(left.unwrap() + right.unwrap()),
                Tokens::Minus => return Ok(left.unwrap() - right.unwrap()),
                Tokens::Star => return Ok(left.unwrap() * right.unwrap()),
                Tokens::Slash => return Ok(left.unwrap() / right.unwrap()),
                _ => return Err(String::from("Runtime error: binary operation not supported")),
            }
        },
        Some(Expr::Unary(op, r)) => {
            let right = eval(*r);
            if let Err(s) = right {
                return Err(s);
            }
            match op.token_type {
                Tokens::Minus => return Ok(- right.unwrap()),
                _ => return Err(String::from("Runtime error: unary operation not supported")),
            }
        },
        Some(Expr::Group(e)) => eval(*e),
        Some(Expr::Primary(f)) => Ok(f),
        None => Err(String::from("Runtime error"))
    }
}

fn main() {

    let mut token_list: Vec<Token> = Vec::new();

    loop {
        token_list.clear();
        let mut buf = String::new();
        
        print!("> ");
        io::stdout().flush().expect("Failed to flush stdout");
        io::stdin().read_line(&mut buf).expect("Failed to read input line");
        
        let buf = buf.trim();
        if buf.len() == 0 {
            continue;
        }

        lexer(&mut token_list, buf);
        println!("{:?}", token_list);

        let expr = parse(&mut token_list);
        // println!("{:?}", expr);

        let res = eval(expr);
        match res {
            Ok(f) => println!("{}", f),
            Err(s) => println!("{}", s)
        }
    }
}
