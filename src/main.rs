use std::process::ExitCode;

mod driver;

fn main() -> ExitCode {
    let result = driver::run();

    if let Err(err) = result {
        eprintln!("{}", err);
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
