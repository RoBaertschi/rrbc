use std::error::Error;

use rbc::driver;

fn main() -> Result<(), Box<dyn Error>> {
    driver::run()?;

    Ok(())
}
