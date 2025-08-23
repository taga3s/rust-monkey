[private]
@default: help

# show help message
@help:
    echo "Usage: just <recipe>"
    echo ""
    just --list

run PACKAGE_NAME:
    cargo run -p {{PACKAGE_NAME}}

test PACKAGE_NAME:
    cargo test -p {{PACKAGE_NAME}}

