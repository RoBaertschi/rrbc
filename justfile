tests := ".tests"
wacc-tests := tests + "/writing-a-c-compiler-tests"

test_compiler := "./.tests/writing-a-c-compiler-tests/test_compiler"

exists := if path_exists(wacc-tests) == "false" { "just download-tests" } else { "echo Tests already downloaded" }

features := ""

default: test

download-tests:
    mkdir -p .tests
    git clone https://github.com/nlsandler/writing-a-c-compiler-tests {{wacc-tests}}

clean-tests:
    rm -rf .tests

build:
    cargo build --no-default-features {{features}}

clean: clean-tests
    cargo clean

rebuild:
    cargo clean
    just build

setup:
    just download-tests
    {{test_compiler}} --check-setup

chapter := "10"

test *EXTRA="--extra-credit":
    just build
    {{exists}}

    @echo "Running Tests, this can take a while..."
    {{test_compiler}} ./target/debug/rrbc --chapter {{chapter}} {{EXTRA}}

simple:
    cargo r -- simple.c
    ./simple

parse_simple:
    cargo r --no-default-features -- simple.c --parse

parser_tests:
    cargo t --no-default-features -p rrbc-parser

validate_simple:
    cargo r --no-default-features --features validate -- simple.c --validate

validate_tests:
    cargo t -p rrbc-semantic-analysis


tackygen_simple:
    cargo r --no-default-features --features tackygen -- simple.c --tacky

tackygen_tests:
    cargo t -p rrbc-tackygen

asmgen_simple:
    cargo r --no-default-features --features asmgen -- simple.c -S

asmgen_tests:
    cargo t -p rrbc-asmgen


emit_simple:
    cargo r -- simple.c

emit_tests:
    cargo t --workspace
