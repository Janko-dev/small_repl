# Small REPL
Tiny REPL (Read, Evaluate, Print, Loop) environment for evaluating expressions in Rust. The application does not depend on external crates (std only). Input lines are read from stdin and lexical analysis is performed to generate a list of legal tokens. Thereafter, a bottom-up recursive descent parser generates AST (Abstract Syntax Tree) nodes to form the hierarchy and precedence of expressions. This is passed to an evaluator that generates a result value, of which there can be two possible types of values. A 64 bit floating point number value or an array of 64 bit floating point numbers. Equality and comparison expressions evaluate to 1 when true and 0 when false.

## Grammar

``` ebnf
Expression   =   ListCompr | Equality ;
Equality     =   Comparison (("!=" | "==") Comparison)* ;
Comparison   =   Term (("<" | "<=" | ">" | ">=") Term)* ;
Term         =   Factor (("+" | "-") Factor)* ;
Factor       =   Exponent (("*" | "/") Exponent)* ;
Exponent     =   Unary ("**" Unary)* ;
Unary        =   (("-" | "!") Unary)* | Primary ;
Primary      =   Number | Identifier | "(" Equality ")" ;
Number       =   digit* ("." digit*) ;
Identifier   =   alphabetic (alphanum*) ;

ListCompr    =   "[" Equality "for" Identifier "in" Equality ".." Equality "]" ;
```

# Quick start + examples
``` c#
$ cargo run --release
Welcome to Small REPL
> 3 + 2
5
> [x+3**3 for x in 0..20)]
[27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46]
> [x < 4 for x in -3..7] 
[1, 1, 1, 1, 1, 1, 1, 0, 0, 0]
> 10**(3**2) 
1000000000
> 3**2**0.5
3
```
