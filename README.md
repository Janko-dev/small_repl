# Small REPL
Tiny REPL (Read, Evaluate, Print, Loop) environment for evaluating expressions.
The application is written in Rust. Input lines are read from stdin, lexical analysis is performed to generate a list of tokens. Thereafter, a bottom-up recursive descent parser generates the AST (Abstract Syntax Tree). This is passed to an evaluator that generates a result value.

Currently supported expressions:
- add/subtract (e.g. 1+2, 3-1)
- multiply/division (e.g. 10*7, 80/5)
- raising to the power (e.g. 3\**2, 2\**10)
- parenthisized grouping (e.g. 2*(3-9))
- equality/inequality (e.g. 2 == 2, 5+1 != 5)
- comparison (e.g. 3<6, 7<=7, 10>3, 100>=9)
- unary (e.g. -4, !(2 == 3))
- list comprehension (e.g. [x\*x for x in 0..5] yields [0, 1, 4, 9, 16])

# Quick start
```
$ cargo run
Welcome to Small REPL
> 3 + 2
5
> [x+3**3 for x in 0..20)]
[27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46]
```
