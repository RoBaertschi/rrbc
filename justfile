tests := ".tests"
wacc-tests := tests + "/writing-a-c-compiler-tests"
chapter := "9"

test_compiler := "./.tests/writing-a-c-compiler-tests/test_compiler"

exists := if path_exists(wacc-tests) == "false" { "just download-tests" } else { "echo Tests already downloaded" }


default:
    just test

download-tests:
    mkdir -p .tests
    git clone https://github.com/nlsandler/writing-a-c-compiler-tests {{wacc-tests}}

clean-tests:
    rm -rf .tests

build:
    cargo build

clean:
    just clean-tests
    cargo clean

rebuild:
    cargo clean
    just build

setup:
    just download-tests
    {{test_compiler}} --check-setup

test:
    just build
    {{exists}}

    @echo "Running Tests, this can take a while..."
    {{test_compiler}} ./target/debug/rrbc --chapter {{chapter}} --extra-credit
