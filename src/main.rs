use std::{env, error::Error};

use rbc::driver;

fn main() -> Result<(), Box<dyn Error>> {
    let opts = driver::Options::parse_args(env::args());

    opts.run_assembler()?;

    Ok(())
}
