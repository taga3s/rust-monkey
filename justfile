[private]
@default: help

# show help message
@help:
    echo "Usage: just <recipe>"
    echo ""
    just --list

run-repl:
    cargo run -p repl

test PACKAGE_NAME:
    cargo test -p {{PACKAGE_NAME}}

