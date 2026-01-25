# monkey-rs

Rust implementation of the Monkey language, based on the book *[Writing An Interpreter In Go](https://interpreterbook.com/)*.

## features
- Tree-Walking Interpreter
  - interprets the AST on the fly
- No external dependencies
  - only uses Rust standard library

## Usage

### REPL
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

### Runner

```bash
just run <file_path>
```

### Convert to wasm

```bash
just build-wasm
```
