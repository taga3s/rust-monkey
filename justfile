[private]
@default: help

# show help message
@help:
    echo "Usage: just <recipe>"
    echo ""
    just --list

repl:
    cargo run -p repl

test:
    cargo test

