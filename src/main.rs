use std::{io::{self, Write}, str::{Chars}, iter::Peekable, slice::Iter, fmt::Display};

// pair(1, pair(2, pair(3)))

#[derive(Debug, Clone, Copy, PartialEq)]
enum Token {
    Plus,
    Minus,
    Star,
    Slash,
    Pow,
    Bang,

    EqualEqual,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Dot,

    Number(f64),
    Identifier(usize),

    For,
    In,
}

#[derive(Debug)]
enum Expr {
    Binary(Box<Option<Expr>>, Token, Box<Option<Expr>>),
    Unary(Token, Box<Option<Expr>>),
    Group(Box<Option<Expr>>),
    Primary(Token),
    List(Box<Option<Expr>>, Token, Box<Option<Expr>>, Box<Option<Expr>>),
}

#[derive(Debug)]
enum Value {
    Float(f64),
    List(Vec<Value>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Float(n) => write!(f, "{}", n),
            Value::List(lst) => {
                write!(f, "[")?;
                for (i, val) in lst.iter().enumerate() {
                    match val {
                        Value::Float(n) => { write!(f, "{}", n)?; },
                        _ => { write!(f, "")?; },
                    };
                    if i < lst.len()-1 {
                        write!(f, ", ")?;
                    }                        
                }
                write!(f, "]")?;
                Ok(())
            },
        }
    }
}

fn number(list: &mut Vec<Token>, chars: &mut Peekable<Chars>){
    let mut lexeme = String::new();
    while let Some(c @ '0'..='9') = chars.peek() {
        lexeme.push(*c);
        chars.next();
    }

    let rest = chars.clone().collect::<String>();
    if rest.starts_with(".."){
        let num: f64 = lexeme.parse().expect("Failed to parse number");
        list.push(Token::Number(num));
        return;
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

fn identifier(list: &mut Vec<Token>, interned: &mut Vec<String>, chars: &mut Peekable<Chars>, opt_keyword: Option<(&str, Token)>){
    let mut lexeme = String::new();
    while let Some(c @ 'a'..='z') |
              Some(c @ 'A'..='Z') |
              Some(c @ '_') |
              Some(c @ '0'..='9') = chars.peek() 
    {
        lexeme.push(*c);
        chars.next();
    
    }
    if let Some((keyword, token)) = opt_keyword {
        if lexeme == keyword {
            list.push(token);
            return;
        } 
    } 
    interned.push(lexeme);
    list.push(Token::Identifier(interned.len()-1));
}

fn add_token(list: &mut Vec<Token>, chars: &mut Peekable<Chars>, token_type: Token){
    chars.next();
    list.push(token_type);
}

fn add_token_alt(list: &mut Vec<Token>, chars: &mut Peekable<Chars>, token_type: Token, alt: char, alt_token: Token){
    chars.next();
    if let Some(c) = chars.peek() {
        if *c == alt {
            list.push(alt_token);
            chars.next();
        } else {
            list.push(token_type);
        }
    }
}

// [x for x in 1..10]
fn lexer(list: &mut Vec<Token>, interned: &mut Vec<String>, input: &str){
    let mut chars = input.chars().peekable();
    loop {
        match chars.peek() {
            Some('f') => identifier(list, interned, &mut chars, Some(("for", Token::For))),
            Some('i') => identifier(list, interned, &mut chars, Some(("in", Token::In))),
            Some('a'..='z') | Some('A'..='Z') | Some('_') => identifier(list, interned, &mut chars, None),
            Some('0'..='9') => number(list, &mut chars),
            Some('+') => add_token(list, &mut chars, Token::Plus),
            Some('-') => add_token(list, &mut chars, Token::Minus),
            Some('*') => add_token_alt(list, &mut chars, Token::Star, '*', Token::Pow),
            Some('/') => add_token(list, &mut chars, Token::Slash),

            Some('!') => add_token_alt(list, &mut chars, Token::Bang, '=', Token::BangEqual),
            Some('<') => add_token_alt(list, &mut chars, Token::Less, '=', Token::LessEqual),
            Some('>') => add_token_alt(list, &mut chars, Token::Greater, '=', Token::GreaterEqual),
            
            Some('(') => add_token(list, &mut chars, Token::OpenParen),
            Some(')') => add_token(list, &mut chars, Token::CloseParen),

            Some('[') => add_token(list, &mut chars, Token::OpenBracket),
            Some(']') => add_token(list, &mut chars, Token::CloseBracket),
            Some('.') => add_token(list, &mut chars, Token::Dot),
            
            Some('=') => {
                chars.next();
                if let Some(c) = chars.next() {
                    if c == '=' {
                        list.push(Token::EqualEqual);
                    } else {
                        println!("Unknown character: {}", c);
                    }
                }
            },
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

fn expect(list: &mut Peekable<Iter<Token>>, expected: Token, err_msg: &str){
    match list.peek() {
        Some(t) => {
            if **t != expected {
                println!("{}", err_msg);
                return;
            }
            list.next();
        },
        None => {},
    }
}

fn parse(list: &mut Vec<Token>) -> Option<Expr> {
    let mut tokens = list.iter().peekable();
    match tokens.peek() {
        Some(Token::OpenBracket) => list_comprehension(&mut tokens),
        Some(_) => equality(&mut tokens),
        None => None,
    }
    // equality(&mut list.iter().peekable())
}

// [expr for id in 0..10]
fn list_comprehension(list: &mut Peekable<Iter<Token>>) -> Option<Expr> {
    list.next();
    let expr = equality(list);
    expect(list, Token::For, "Expected for keyword");
    let id = match list.next() {
        Some(Token::Identifier(n)) => Token::Identifier(*n),
        Some(other) => {
            println!("Expected identifier but got {:?}", other);
            return None;
        },
        None => {
            println!("Expected identifier but got None");
            return None;
        }
    };
    expect(list, Token::In, "Expected in keyword");
    let min = equality(list);
    expect(list, Token::Dot, "Expected '.'");
    expect(list, Token::Dot, "Expected '.'");
    let max = equality(list);
    expect(list, Token::CloseBracket, "Expected ']'");
    Some(Expr::List(Box::new(expr), id, Box::new(min), Box::new(max)))
}

fn equality(list: &mut Peekable<Iter<Token>>) -> Option<Expr> {
    let mut left = comparison(list);

    loop {
        match list.peek() {
            Some(Token::BangEqual) | Some(Token::EqualEqual) => {
                let op = list.next().unwrap().clone();
                let right = comparison(list);
                left = Some(Expr::Binary(Box::new(left), op, Box::new(right)));
            },
            _ => break,
        }
    };
    left
}

fn comparison(list: &mut Peekable<Iter<Token>>) -> Option<Expr> {
    let mut left = term(list);

    loop {
        match list.peek() {
            Some(Token::Less) | Some(Token::LessEqual) |
            Some(Token::Greater) | Some(Token::GreaterEqual) => {
                let op = list.next().unwrap().clone();
                let right = term(list);
                left = Some(Expr::Binary(Box::new(left), op, Box::new(right)));
            },
            _ => break,
        }
    };
    left
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
    let mut left = exponent(list);

    loop {
        match list.peek() {
            Some(Token::Star) | Some(Token::Slash) => {
                let op = list.next().unwrap().clone();
                let right = exponent(list);
                left = Some(Expr::Binary(Box::new(left), op, Box::new(right)));
            },
            _ => break,
        }
    };
    left
}

fn exponent(list: &mut Peekable<Iter<Token>>) -> Option<Expr> {
    let mut left = unary(list);

    loop {
        match list.peek() {
            Some(Token::Pow) => {
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
            Some(Token::Bang) => {
                let op = list.next().unwrap().clone();
                return Some(Expr::Unary(op, Box::new(unary(list))));
            }
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
        Some(Token::Identifier(_)) => {
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

fn eval(expr: &Option<Expr>, interned: &Vec<String>, binding: &Option<(String, f64)>) -> Result<Value, String>{
    match expr {
        Some(Expr::Binary(l, op, r)) => {
            let left = eval(&*l, interned, binding);
            let left = match left {
                Err(s) => return Err(s),
                Ok(Value::List(_)) => return Err("Left expression must be f64".to_string()),
                Ok(Value::Float(f)) => f,
            };

            let right = eval(&*r, interned, binding);
            let right = match right {
                Err(s) => return Err(s),
                Ok(Value::List(_)) => return Err("Right expression must be f64".to_string()),
                Ok(Value::Float(f)) => f,
            };
            
            match op {
                Token::Plus => return Ok(Value::Float(left + right)),
                Token::Minus => return Ok(Value::Float(left - right)),
                Token::Star => return Ok(Value::Float(left * right)),
                Token::Slash => return Ok(Value::Float(left / right)),
                Token::Pow => return Ok(Value::Float(left.powf(right))),

                Token::Less => return Ok(Value::Float((left < right) as i32 as f64)),
                Token::LessEqual => return Ok(Value::Float((left <= right) as i32 as f64)),
                Token::Greater => return Ok(Value::Float((left > right) as i32 as f64)),
                Token::GreaterEqual => return Ok(Value::Float((left >= right) as i32 as f64)),
                Token::EqualEqual => return Ok(Value::Float((left == right) as i32 as f64)),
                Token::BangEqual => return Ok(Value::Float((left != right) as i32 as f64)),

                _ => return Err(String::from("Runtime error: binary operation not supported")),
            }
        },
        Some(Expr::Unary(op, r)) => {
            let right = eval(&*r, interned, binding);
            let right = match right {
                Err(s) => return Err(s),
                Ok(Value::List(_)) => return Err("Right expression must be f64".to_string()),
                Ok(Value::Float(f)) => f,
            };
            match op {
                Token::Minus => return Ok(Value::Float(- right)),
                Token::Bang => return Ok(Value::Float((right == 0.0) as i32 as f64)),
                _ => return Err(String::from("Runtime error: unary operation not supported")),
            }
        },
        Some(Expr::Group(e)) => eval(&*e, interned, binding),
        Some(Expr::Primary(Token::Number(f))) => Ok(Value::Float(*f)),
        Some(Expr::Primary(Token::Identifier(n))) => {
            match binding {
                Some((id, f)) => {
                    if interned[*n] == *id {
                        return Ok(Value::Float(*f));
                    } else {
                        return Err(format!("Runtime error: No binding made for {}", interned[*n]))
                    }
                },
                None => return Err(format!("Runtime error: No binding made for {}", interned[*n])),
            }
        },
        Some(Expr::List(expr, id, min, max)) => {
            let min_range = eval(&*min, interned, binding);
            let min_range = match min_range {
                Err(s) => return Err(s),
                Ok(Value::List(_)) => return Err("Left expression must be f64".to_string()),
                Ok(Value::Float(f)) => f,
            };

            let max_range = eval(&*max, interned, binding);
            let max_range = match max_range {
                Err(s) => return Err(s),
                Ok(Value::List(_)) => return Err("Right expression must be f64".to_string()),
                Ok(Value::Float(f)) => f,
            };

            let id = match id {
                Token::Identifier(n) => &interned[*n],
                _ => return Err("identifier in list construction must be a string".to_string()), 
            };

            let mut lst: Vec<Value> = Vec::new();
            for i in min_range as i32..max_range as i32 {
                let res = match eval(&*expr, interned, &Some((id.to_string(), i as f64))) {
                    Err(s) => return Err(s),
                    Ok(f) => f,
                };
                lst.push(res);
            }

            Ok(Value::List(lst))

        },
        Some(_) => Err(String::from("Runtime error: unknown expression")),
        None => Err(String::from("Runtime error")),
    }
}

fn main() {

    let mut token_list: Vec<Token> = Vec::new();
    let mut interned: Vec<String> = Vec::new();

    println!("Welcome to Small REPL");
    loop {
        token_list.clear();
        interned.clear();

        let mut buf = String::new();
        
        print!("> ");
        io::stdout().flush().expect("Failed to flush stdout");
        io::stdin().read_line(&mut buf).expect("Failed to read input line");
        
        let buf = buf.trim();
        if buf.len() == 0 {
            continue;
        }

        lexer(&mut token_list, &mut interned, buf);
        // println!("{:?}", token_list);

        let expr = parse(&mut token_list);
        // println!("{:?}", expr);

        let res = eval(&expr, &interned, &None);
        match res {
            Ok(val) => println!("{}", val),
            Err(s) => println!("{}", s)
        }
    }
}
