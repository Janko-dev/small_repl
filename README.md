# Small REPL
Tiny REPL (Read, Evaluate, Print, Loop) environment for evaluating expressions. The application does not depend on external crates (std only). Input lines are read from stdin and lexical analysis is performed to generate a list of legal tokens. Thereafter, a bottom-up recursive descent parser generates AST (Abstract Syntax Tree) nodes to form the hierarchy and precedence of expressions. This is passed to an evaluator that generates a result value, of which there can be two possible types of values. A 64 bit floating point number value or an array of 64 bit floating point numbers. 

## Grammar
The following grammar depicts the parsing strategy that is implemented. Basic arithmetic expressions are implemented with their corresponding precedence (i.e. plus, minus, multiply, divide, exponentiation, unary minus). Furthermore, equality and comparison expressions are implemented and will yield a numeric value of 1 when true and 0 when false. The most basic of the control flow constructs, i.e. And-operator and Or-operator, are implemented, which will short circuit. Lastly, a python style basic list comprehension expression is implemented that takes an expression, optionally including an identifier binding, and maps a range of values to the bound identifier, yielding a list of numeric values.
``` ebnf
Expression   =   ListCompr | Equality ;
Or           =   And (("||") And)* ;
And          =   Equality (("&&") Equality)* ;
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
> [(x < 7 && x > 2) * x for x in 0..10]  
[0, 0, 0, 3, 4, 5, 6, 0, 0, 0]
```
