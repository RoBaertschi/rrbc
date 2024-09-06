use std::{error::Error, process::ExitCode};

use rbc::driver;

fn main() -> ExitCode {
    let result = driver::run();

    if let Err(err) = result {
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
