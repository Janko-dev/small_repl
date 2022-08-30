use std::{io::{self, Write}, str::{Chars}, iter::Peekable, slice::Iter};

#[derive(Debug, Clone, Copy)]
enum Token {
    Plus,
    Minus,
    Star,
    Slash,
    OpenParen,
    CloseParen,
    Number(f64),
}

#[derive(Debug)]
enum Expr {
    Binary(Box<Option<Expr>>, Token, Box<Option<Expr>>),
    Unary(Token, Box<Option<Expr>>),
    Group(Box<Option<Expr>>),
    Primary(Token),
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
    let num: f64 = lexeme.parse().expect("Failed to parse number");
    list.push(Token::Number(num));
}

fn add_operator(list: &mut Vec<Token>, chars: &mut Peekable<Chars>, token_type: Token){
    chars.next();
    list.push(token_type);
}

fn lexer(list: &mut Vec<Token>, input: &str){
    let mut chars = input.chars().peekable();
    
    loop {
        match chars.peek() {
            Some('0'..='9') => number(list, &mut chars),
            Some('+') => add_operator(list, &mut chars, Token::Plus),
            Some('-') => add_operator(list, &mut chars, Token::Minus),
            Some('*') => add_operator(list, &mut chars, Token::Star),
            Some('/') => add_operator(list, &mut chars, Token::Slash),
            Some('(') => add_operator(list, &mut chars, Token::OpenParen),
            Some(')') => add_operator(list, &mut chars, Token::CloseParen),
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
            Some(Token::Plus) | Some(Token::Minus) => {
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
            Some(Token::Star) | Some(Token::Slash) => {
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
            Some(Token::Minus) => {
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
        Some(Token::Number(_)) => {
            let val = list.next().unwrap().clone();
            Some(Expr::Primary(val))
        },
        Some(Token::OpenParen) => {
            list.next();
            let res = Expr::Group(Box::new(term(list)));
            
            if let Some(Token::CloseParen) = list.next() {
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
            match op {
                Token::Plus => return Ok(left.unwrap() + right.unwrap()),
                Token::Minus => return Ok(left.unwrap() - right.unwrap()),
                Token::Star => return Ok(left.unwrap() * right.unwrap()),
                Token::Slash => return Ok(left.unwrap() / right.unwrap()),
                _ => return Err(String::from("Runtime error: binary operation not supported")),
            }
        },
        Some(Expr::Unary(op, r)) => {
            let right = eval(*r);
            if let Err(s) = right {
                return Err(s);
            }
            match op {
                Token::Minus => return Ok(- right.unwrap()),
                _ => return Err(String::from("Runtime error: unary operation not supported")),
            }
        },
        Some(Expr::Group(e)) => eval(*e),
        Some(Expr::Primary(Token::Number(f))) => Ok(f),
        Some(_) => Err(String::from("Runtime error: unknown expression")),
        None => Err(String::from("Runtime error")),
    }
}

fn main() {

    let mut token_list: Vec<Token> = Vec::new();
    println!("Welcome to Small REPL");
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
        // println!("{:?}", token_list);

        let expr = parse(&mut token_list);
        // println!("{:?}", expr);

        let res = eval(expr);
        match res {
            Ok(f) => println!("{}", f),
            Err(s) => println!("{}", s)
        }
    }
}
