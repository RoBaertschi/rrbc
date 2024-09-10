use std::{
    env::{self, Args},
    error::Error,
    fmt::Display,
    fs,
    io::{self},
    path::PathBuf,
    process::{self, ChildStderr, ChildStdout, Command},
    str::FromStr,
};

use crate::{
    ast,
    //tackler, tacky,
    //assembly, ast, codegen,
    //emit::EmitAsm,
    lexer::{self, LexerError},
    parser::{self, ParserError},
};

#[derive(Debug)]
pub enum DriverExecutionError {
    IoError(io::Error),
    PreprocessorFailed(Option<ChildStderr>, Option<ChildStdout>),
    /// The preprocessor did not create a file.
    PreprocessorNoFile,
    AssemblerFailed(Option<ChildStderr>, Option<ChildStdout>),
    AssemblerNoFile,
    Lexer(LexerError),
    Parser(ParserError),
}

impl Display for DriverExecutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error: {:?}", self)
    }
}

impl Error for DriverExecutionError {}

impl From<io::Error> for DriverExecutionError {
    fn from(value: io::Error) -> Self {
        Self::IoError(value)
    }
}

impl From<lexer::LexerError> for DriverExecutionError {
    fn from(value: lexer::LexerError) -> Self {
        Self::Lexer(value)
    }
}

impl From<ParserError> for DriverExecutionError {
    fn from(value: ParserError) -> Self {
        Self::Parser(value)
    }
}

#[derive(Default)]
pub enum Stage {
    #[default]
    Compile,
    Lex,
    Parse,
    Tacky,
    Codegen,
    Assembly,
}

#[derive(Default)]
pub struct Options {
    stage: Stage,
    input_file: PathBuf,
    preprocessed_file: PathBuf,
    assembly_file: PathBuf,
    output_file: PathBuf,

    lexer: Option<lexer::Lexer>,
}

fn print_help(file: Option<String>) -> ! {
    println!(
        "{} FILE [--lex | --parse | --codegen | -S]",
        file.unwrap_or("rbc (could not extract executable path.)".to_string())
    );
    process::exit(1)
}

fn is_flag(string: &str) -> Option<Stage> {
    match string {
        "--lex" => Some(Stage::Lex),
        "--parse" => Some(Stage::Parse),
        "--codegen" => Some(Stage::Codegen),
        "-S" => Some(Stage::Assembly),
        "--tacky" => Some(Stage::Tacky),
        _ => None,
    }
}

impl Options {
    /// This function will exit if the args don't match what was expected.
    pub fn parse_args(mut args: Args) -> Self {
        let target = args.next();

        if target.is_none() {
            println!("rbc was called without any args, even missing the executable name itself");
            print_help(target);
        }

        if args.len() < 1 || args.len() > 2 {
            println!("Invalid amount of arguments, expected 1-2 arguments.");

            print_help(target);
        }

        let mut goal: Stage = Default::default();
        let mut file_path: Option<PathBuf> = None;

        for arg in args {
            if let Some(found_goal) = is_flag(&arg) {
                goal = found_goal;
                continue;
            }

            if file_path.is_some() {
                println!("rbc only accecpts and file or the flag was not regognized");
                print_help(target);
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
            print_help(target);
        }

        let input_file = file_path.unwrap();
        let mut preprocessed_file = input_file.clone();
        preprocessed_file.set_extension("i");
        let mut assembly_file = input_file.clone();
        assembly_file.set_extension("s");
        let mut output_file = input_file.clone();
        output_file.set_extension("");

        return Self {
            stage: goal,
            input_file,
            preprocessed_file,
            assembly_file,
            output_file,
            ..Default::default()
        };
    }

    pub fn run_preprocessor(&self) -> Result<(), DriverExecutionError> {
        let mut command = Command::new("gcc")
            .arg("-E")
            .arg("-P")
            .arg(self.input_file.as_os_str())
            .arg("-o")
            .arg(self.preprocessed_file.as_os_str())
            .spawn()?;

        let exit_code = command.wait()?;

        if !exit_code.success() {
            return Err(DriverExecutionError::PreprocessorFailed(
                command.stderr,
                command.stdout,
            ));
        }

        if !self.preprocessed_file.exists() {
            return Err(DriverExecutionError::PreprocessorNoFile);
        }

        Ok(())
    }

    pub fn run_assembler(&self) -> Result<(), DriverExecutionError> {
        let mut command = Command::new("gcc")
            .arg(self.assembly_file.as_os_str())
            .arg("-o")
            .arg(self.output_file.as_os_str())
            .spawn()?;

        let exit_code = command.wait()?;

        if !exit_code.success() {
            return Err(DriverExecutionError::AssemblerFailed(
                command.stderr,
                command.stdout,
            ));
        }

        if !self.output_file.exists() {
            return Err(DriverExecutionError::AssemblerNoFile);
        }

        Ok(())
    }

    pub fn run_lexer(&mut self) -> Result<(), DriverExecutionError> {
        let input = fs::read_to_string(&self.preprocessed_file)?;

        let lexer = lexer::Lexer::new(input);

        if let Stage::Lex = self.stage {
            for tok in lexer {
                tok?;
            }
        } else {
            self.lexer = Some(lexer);
        }

        Ok(())
    }

    pub fn run_parser(&mut self) -> Result<ast::Program, DriverExecutionError> {
        let lexer = self.lexer.take().unwrap();

        let mut parser = parser::Parser::try_build(lexer)?;
        let program = parser.parse_program()?;

        if let Stage::Parse = self.stage {
            println!("{:#?}", program);
        }

        Ok(program)
    }

    ///// Runs the code gen without creating the file.
    //pub fn run_code_gen(
    //    &self,
    //    ast_program: tacky::Program,
    //) -> Result<assembly::Program, DriverExecutionError> {
    //    let program = codegen::code_generation(ast_program);
    //
    //    if let Stage::Codegen = self.stage {
    //        println!("{:#?}", program);
    //    }
    //
    //    Ok(program)
    //}
    //
    //pub fn run_assembly_emission(
    //    &self,
    //    program: assembly::Program,
    //) -> Result<(), DriverExecutionError> {
    //    fs::write(&self.assembly_file, program.emit(0))?;
    //
    //    Ok(())
    //}
}

pub fn run() -> Result<(), DriverExecutionError> {
    let args = env::args();
    let mut opts = Options::parse_args(args);

    opts.run_preprocessor()?;

    opts.run_lexer()?;

    if let Err(err) = fs::remove_file(&opts.preprocessed_file) {
        eprintln!(
            "WARN: Could not remove the file {:?} due to {}, continuing",
            &opts.preprocessed_file, err
        );
    }

    if let Stage::Lex = opts.stage {
        return Ok(());
    }

    let program = opts.run_parser()?;

    if let Stage::Parse = opts.stage {
        return Ok(());
    }

    //let program = tackler::emit_tacky_program(program);

    //if let Stage::Tacky = opts.stage {
    //    println!("{:#?}", program);
    //    return Ok(());
    //}
    //
    //let program = opts.run_code_gen(program)?;
    //
    //if let Stage::Codegen = opts.stage {
    //    return Ok(());
    //}
    //
    //opts.run_assembly_emission(program)?;

    if let Stage::Compile = opts.stage {
        opts.run_assembler()?;

        if let Err(err) = fs::remove_file(&opts.assembly_file) {
            eprintln!(
                "WARN: Could not remove the file {:?} due to {}, finishing...",
                &opts.assembly_file, err
            )
        }
    }

    Ok(())
}
