use std::env;

use rbc::driver;

fn main() {
    driver::parse_args(env::args());
}
