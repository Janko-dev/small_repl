# Small REPL
Tiny REPL (Read, Evaluate, Print, Loop) environment for evaluating expressions.
The application is written in Rust. Input lines are read from stdin, lexical analysis is performed to generate a list of tokens. Thereafter, a bottom-up recursive descent parser generates the AST (Abstract Syntax Tree). This is passed to an evaluator that generates a result value.

# Quick start
```
$ cargo run
Welcome to Small REPL
> 3 + 2
5
```
