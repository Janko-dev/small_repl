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
    Binary(Box<Expr>, Token, Box<Expr>),
    Unary(Token, Box<Expr>),
    Group(Box<Expr>),
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
            Some(_) => break,
            None => {
                break;
            }
        }
    }
}

fn parse(list: &mut Vec<Token>) -> Expr {
    term(&mut list.iter().peekable())
}

fn term(list: &mut Peekable<Iter<Token>>) -> Expr {
    let left = factor(list);

    loop {
        match list.peek() {
            Some(Token { lexeme: _, token_type: Tokens::Plus }) |
            Some(Token { lexeme: _, token_type: Tokens::Minus }) => {
                let op = list.next().unwrap().clone();
                let right = factor(list);
                return Expr::Binary(Box::new(left), op, Box::new(right));
            },
            _ => break,
        }
    };
    left
}

fn factor(list: &mut Peekable<Iter<Token>>) -> Expr {
    let left = unary(list);

    loop {
        match list.peek() {
            Some(Token { lexeme: _ , token_type: Tokens::Star }) |
            Some(Token { lexeme: _, token_type: Tokens::Slash }) => {
                let op = list.next().unwrap().clone();
                let right = unary(list);
                return Expr::Binary(Box::new(left), op, Box::new(right));
            },
            _ => break,
        }
    };
    left
}

fn unary(list: &mut Peekable<Iter<Token>>) -> Expr {
    loop {
        match list.peek() {
            Some(Token { lexeme: _, token_type: Tokens::Minus }) => {
                let op = list.next().unwrap().clone();
                return Expr::Unary(op, Box::new(unary(list)));
            },
            _ => break,
        }
    };
    return primary(list);
}

fn primary(list: &mut Peekable<Iter<Token>>) -> Expr {
    match list.peek() {
        Some(Token { lexeme , token_type: Tokens::Number }) => {
            list.next();
            let value: f64 = lexeme.parse().expect("failed to parse number");
            Expr::Primary(value)
        },
        Some(Token { lexeme: _, token_type: Tokens::OpenParen }) => {
            list.next();
            let res = Expr::Group(Box::new(term(list)));
            
            if let Some(Token { lexeme: _, token_type: Tokens::CloseParen }) = list.next() {
                return res;
            } else {
                panic!("closing paren missing");
            }
        },
        _ => {
            println!("Error");
            panic!();
        }
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

        lexer(&mut token_list, buf.trim());
        
        println!("{:?}", token_list);

        let expr = parse(&mut token_list);
        println!("{:?}", expr);
    }
}
