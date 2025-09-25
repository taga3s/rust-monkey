# rust-monkey

Rust implementation of the Monkey programming language, based on the book *[Writing An Interpreter In Go](https://interpreterbook.com/)*.

This project is work in progress.

## features
- Tree-Walking Interpreter
  - interprets the AST on the fly
- No external dependencies
  - only uses Rust standard library

## REPL
1. Run the REPL with `just repl` command.

```bash
just repl
```

2. Type in Monkey code and press Enter to execute.

```monkey
>> let add = fn(a, b) { a + b; };
>> add(10, 15);
25
```
