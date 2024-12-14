use std::{
    env::{self, Args},
    fs,
    io::{self},
    path::PathBuf,
    process::{self, ChildStderr, ChildStdout, Command},
    str::FromStr,
};

use thiserror::Error;

use crate::{
    ast,
    lexer::{self, LexerError},
    parser::{self, ParserError},
};

#[cfg(feature = "validate")]
use crate::semantic_analysis::{
    label_resolution::LabelResolutionError,
    loop_labeling::LoopLabelingError,
    switch_resolution::SwitchResolutionError,
    variable_resolution::{self, VariableResolutionError},
};

#[cfg(feature = "codegen")]
use crate::{assembly, codegen, emit::EmitAsm};

#[cfg(feature = "tacky")]
use crate::{tackler, tacky};

#[derive(Error, Debug)]
pub enum DriverExecutionError {
    #[error("{0}")]
    IoError(#[from] io::Error),
    #[error("Preprocessor Failed:\nStderr: {0:?}\nStdout: {1:?}")]
    PreprocessorFailed(Option<ChildStderr>, Option<ChildStdout>),
    /// The preprocessor did not create a file.
    #[error("Could not find the file created by the preprocessor.")]
    PreprocessorNoFile,
    #[error("Assembler Failed:\nStderr: {0:?}\nStdout: {1:?}")]
    AssemblerFailed(Option<ChildStderr>, Option<ChildStdout>),
    #[error("Could not find the file created by the assembler.")]
    AssemblerNoFile,
    #[error("{0}")]
    Lexer(#[from] LexerError),
    #[error("{0}")]
    Parser(#[from] ParserError),
    #[cfg(feature = "validate")]
    #[error("{0}")]
    VariableResolutionError(#[from] VariableResolutionError),
    #[cfg(feature = "validate")]
    #[error("{0}")]
    LabelResolutionError(#[from] LabelResolutionError),
    #[cfg(feature = "validate")]
    #[error("{0}")]
    LoopLabelingError(#[from] LoopLabelingError),
    #[cfg(feature = "validate")]
    #[error("{0}")]
    SwitchResolutionError(#[from] SwitchResolutionError),
}

#[derive(Default)]
pub enum Stage {
    #[default]
    Compile,
    Lex,
    Parse,
    Validate,
    Tacky,
    Codegen,
    Assembly,
}

#[derive(Default)]
pub enum ProgramType {
    #[default]
    Executable,
    ObjectFile,
}

pub struct FileSet {
    input_file: PathBuf,
    preprocessed_file: PathBuf,
    output_file: PathBuf,
}

#[derive(Default)]
pub struct Options {
    stage: Stage,
    program_type: ProgramType,
    input_files: Vec<PathBuf>,

    lexer: Option<lexer::Lexer>,
}

fn print_help(file: Option<String>) -> ! {
    println!(
        "{} FILE [--lex | --parse | --validate | --tacky | --codegen | -S | -c]",
        file.unwrap_or("rbc (could not extract executable path.)".to_string())
    );
    process::exit(1)
}

fn is_goal_flag(string: &str) -> Option<Stage> {
    match string {
        "--lex" => Some(Stage::Lex),
        "--parse" => Some(Stage::Parse),
        "--codegen" => Some(Stage::Codegen),
        "-S" => Some(Stage::Assembly),
        "--tacky" => Some(Stage::Tacky),
        "--validate" => Some(Stage::Validate),
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
        let mut program_type: ProgramType = Default::default();
        let mut file_paths: Vec<PathBuf> = vec![];

        for arg in args {
            if let Some(found_goal) = is_goal_flag(&arg) {
                goal = found_goal;
                continue;
            }
            if &arg == "-c" {
                program_type = ProgramType::ObjectFile;
                continue;
            }

            match PathBuf::from_str(&arg) {
                Ok(path) => {
                    if path.exists() {
                        file_paths.push(path);
                    } else {
                        println!("could not find file {}", arg);
                        process::exit(1);
                    }
                }
                Err(err) => {
                    println!("could not parse path {} because of {}", arg, err);
                    process::exit(1);
                }
            }
        }

        if file_paths.is_empty() {
            println!("no input files");
            print_help(target);
        }

        Self {
            stage: goal,
            input_files: file_paths,
            program_type,
            ..Default::default()
        }
    }

    pub fn run_preprocessor(&self, file_set: &FileSet) -> Result<(), DriverExecutionError> {
        let mut command = Command::new("gcc")
            .arg("-E")
            .arg("-P")
            .arg(file_set.input_file.as_os_str())
            .arg("-o")
            .arg(file_set.preprocessed_file.as_os_str())
            .spawn()?;

        let exit_code = command.wait()?;

        if !exit_code.success() {
            return Err(DriverExecutionError::PreprocessorFailed(
                command.stderr,
                command.stdout,
            ));
        }

        if !file_set.preprocessed_file.exists() {
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

    #[cfg(feature = "validate")]
    pub fn run_validator(
        &mut self,
        program: ast::Program,
    ) -> Result<ast::Program, DriverExecutionError> {
        use crate::semantic_analysis::{label_resolution, loop_labeling, switch_resolution};

        let program = label_resolution::resolve_program(program)?;
        let program = loop_labeling::label_program(program)?;
        let program = switch_resolution::resolve_program(program)?;
        Ok(variable_resolution::resolve_program(program)?)
    }

    /// Runs the code gen without creating the file.
    #[cfg(feature = "codegen")]
    pub fn run_code_gen(
        &self,
        ast_program: tacky::Program,
    ) -> Result<assembly::Program, DriverExecutionError> {
        let program = codegen::code_generation(ast_program);

        if let Stage::Codegen = self.stage {
            println!("{:#?}", program);
        }

        Ok(program)
    }

    #[cfg(feature = "codegen")]
    pub fn run_assembly_emission(
        &self,
        program: assembly::Program,
    ) -> Result<(), DriverExecutionError> {
        fs::write(&self.assembly_file, program.emit(0))?;

        Ok(())
    }
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

    let mut program = opts.run_parser()?;

    if let Stage::Parse = opts.stage {
        println!("{:?}", program);
        return Ok(());
    }

    #[cfg(feature = "validate")]
    {
        program = opts.run_validator(program)?;
        _ = program;

        if let Stage::Validate = opts.stage {
            return Ok(());
        }

        #[cfg(feature = "tacky")]
        {
            let program = tackler::emit_tacky_program(program);

            if let Stage::Tacky = opts.stage {
                println!("{:#?}", program);
                return Ok(());
            }

            let program = opts.run_code_gen(program)?;

            if let Stage::Codegen = opts.stage {
                return Ok(());
            }

            opts.run_assembly_emission(program)?;

            if let Stage::Compile = opts.stage {
                opts.run_assembler()?;

                if let Err(err) = fs::remove_file(&opts.assembly_file) {
                    eprintln!(
                        "WARN: Could not remove the file {:?} due to {}, finishing...",
                        &opts.assembly_file, err
                    )
                }
            }
        }
    }

    Ok(())
}
