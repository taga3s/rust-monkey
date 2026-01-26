# monkey-rs

Rust implementation of the Monkey language, based on the book *[Writing An Interpreter In Go](https://interpreterbook.com/)*.

## Usage

### Web playground

The web playground is deployed on https://taga3s.github.io/monkey-rs/.

### REPL (crates/repl)
1. Run the REPL with following command.

```bash
just repl
```

2. Type in Monkey code and press Enter to execute.

```monkey
>> let add = fn(a, b) { a + b; };
>> add(10, 15);
25
```

### Runner (crates/runner)

```bash
just run <file_path>
```
