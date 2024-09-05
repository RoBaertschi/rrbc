use std::{env::Args, path::PathBuf, process, str::FromStr};

#[derive(Default)]
pub enum Goal {
    #[default]
    Compile,
    Lex,
    Parse,
    Codegen,
    Assembly,
}

#[derive(Default)]
pub struct Options {
    goal: Goal,
    input_file: PathBuf,
}

fn print_help(mut args: Args) {
    println!(
        "{} FILE [--lex | --parse | --codegen | -S]",
        args.next()
            .unwrap_or("rbc (could not extract executable path.)".to_string())
    );
}

fn is_flag(string: &str) -> Option<Goal> {
    match string {
        "--lex" => Some(Goal::Lex),
        "--parse" => Some(Goal::Parse),
        "--codegen" => Some(Goal::Codegen),
        "-S" => Some(Goal::Assembly),
        _ => None,
    }
}

pub fn parse_args(args: Args) -> Options {
    if args.len() <= 1 || args.len() > 3 {
        println!("Invalid amount of arguments, expected 1-2 arguments.");

        print_help(args);
        process::exit(1);
    }

    let mut goal: Goal = Default::default();
    let mut file_path: Option<PathBuf> = None;

    let target = &args.next();

    for arg in args {
        if let Some(found_goal) = is_flag(&arg) {
            goal = found_goal;
            continue;
        }

        if file_path.is_some() {
            println!("rbc only accecpts and file or the flag was not regognized");
            print_help(args);
            process::exit(1);
        }
        match PathBuf::from_str(&arg) {
            Ok(path) => {
                if path.exists() {
                    file_path = Some(path);
                } else {
                    println!("could not find file {}", arg);
                    process::exit(1);
                }
            }
            Err(err) => {
                println!(
                    "could not parse path {} because of {}",
                    arg,
                    err.to_string()
                );
                process::exit(1);
            }
        }
    }

    if file_path.is_none() {
        println!("no input file");
        print_help(args);
        process::exit(1);
    }

    let input_file = file_path.unwrap();

    return Options { goal, input_file };
}
