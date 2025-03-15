use std::{
    env::{self, Args},
    fs,
    path::PathBuf,
    process::{self, Command},
};

use anyhow::{bail, Context, Ok};

enum Flag {
    // Goals
    Lex,      // --lex
    Parse,    // --parse
    Validate, // --validate
    Tacky,    // --tacky
    Assembly, // -S
    Codegen,  // --codegen

    // Other
    Output,     // -o
    Help,       // --help
    EmitObject, // -c
    Debug,      // --debug
}

fn flag(string: &str) -> Option<Flag> {
    match string {
        "--lex" => Some(Flag::Lex),
        "--parse" => Some(Flag::Parse),
        "--codegen" => Some(Flag::Codegen),
        "-S" => Some(Flag::Assembly),
        "--tacky" => Some(Flag::Tacky),
        "--validate" => Some(Flag::Validate),
        "-o" => Some(Flag::Output),
        "--help" => Some(Flag::Help),
        "-c" => Some(Flag::EmitObject),
        "--debug" => Some(Flag::Debug),
        _ => None,
    }
}

enum Argument {
    Flag(Flag),
    File(PathBuf),
}

fn argument(arg: String) -> Argument {
    match flag(&arg) {
        Some(flag) => Argument::Flag(flag),
        None => Argument::File(PathBuf::from(arg)),
    }
}

#[derive(Debug)]
enum Goal {
    Lex,
    Parse,
    Validate,
    Tacky,
    Assembly,
    Codegen,
    Compile,
}

fn check_feature_flags_with_goal(goal: &Goal) {
    let ok = match goal {
        Goal::Lex => true,
        Goal::Parse => true,
        Goal::Validate => cfg!(feature = "validate"),
        Goal::Tacky => cfg!(feature = "tackygen"),
        Goal::Assembly => cfg!(feature = "asmgen"),
        Goal::Codegen => cfg!(feature = "asmgen"),
        Goal::Compile => cfg!(feature = "emit"),
    };

    if !ok {
        panic!("tried to run compiler with flag that indicates a feature flag, but the compiler is not build with that feature flag {}",
            match goal {
                Goal::Lex => todo!(),
                Goal::Parse => todo!(),
                Goal::Validate => "validate",
                Goal::Tacky => "tackygen",
                Goal::Assembly => "asmgen",
                Goal::Codegen => "asmgen",
                Goal::Compile => "emit",
            }
        );
    }
}

#[derive(Debug)]
enum ProgramType {
    Object,
    Exectuable,
}

#[derive(Debug)]
struct ProgramArgs {
    goal: Goal,
    inputs: Vec<PathBuf>,
    output: PathBuf,
    program_type: ProgramType,
    debug: bool,
}

const fn get_args() -> &'static [&'static str] {
    if cfg!(feature = "emit") {
        &[
            "--lex",
            "--parse",
            "--validate",
            "--tacky",
            "--codegen",
            "-S",
        ]
    } else if cfg!(feature = "asmgen") {
        &[
            "--lex",
            "--parse",
            "--validate",
            "--tacky",
            "--codegen",
            "-S",
        ]
    } else if cfg!(feature = "tackygen") {
        &["--lex", "--parse", "--validate", "--tacky"]
    } else if cfg!(feature = "validate") {
        &["--lex", "--parse", "--validate"]
    } else {
        &["--lex", "--parse"]
    }
}

const ARGS: &'static [&'static str] = get_args();

fn print_help(file: Option<String>) -> ! {
    println!(
        "{} FILE [{} | -c | -o FILE]",
        file.unwrap_or("rbc (no file arg)".to_string()),
        ARGS.join(" | "),
    );
    process::exit(1)
}

fn parse_program_args(mut args: Args) -> ProgramArgs {
    let file = args.next();
    let mut goal = Goal::Compile;
    let mut inputs = vec![];
    let mut output = Option::None;
    let mut program_type = ProgramType::Exectuable;
    let mut debug = false;

    if args.len() < 1 {
        eprintln!("expected more than one argument");
        print_help(file);
    }

    let mut arguments = args.map(argument);

    loop {
        match arguments.next() {
            Some(arg) => match arg {
                Argument::Flag(flag) => match flag {
                    Flag::Lex => goal = Goal::Lex,
                    Flag::Parse => goal = Goal::Parse,
                    Flag::Validate => goal = Goal::Validate,
                    Flag::Tacky => goal = Goal::Tacky,
                    Flag::Assembly => goal = Goal::Assembly,
                    Flag::Codegen => goal = Goal::Codegen,
                    Flag::Output => match arguments.next() {
                        Some(Argument::File(file)) => output = Some(PathBuf::from(file)),
                        Some(Argument::Flag(_)) => {
                            eprintln!(
                                "expected a argument for the output flag, but got another flag"
                            );
                            print_help(file);
                        }
                        None => {
                            eprintln!("expected a argument for the output flag, but got none");
                            print_help(file);
                        }
                    },
                    Flag::Help => print_help(file),
                    Flag::EmitObject => program_type = ProgramType::Object,
                    Flag::Debug => debug = true,
                },
                Argument::File(path_buf) => {
                    inputs.push(path_buf);
                }
            },
            None => break,
        }
    }

    if inputs.is_empty() {
        eprintln!("not enough input files");
        print_help(file);
    }

    ProgramArgs {
        goal,
        debug,
        output: match output {
            Some(output) => output,
            None => {
                if inputs.len() == 1 {
                    let mut output = inputs[0].clone();
                    output.set_extension(if let ProgramType::Object = &program_type {
                        "o"
                    } else {
                        ""
                    });
                    output
                } else {
                    eprintln!("too many input files to check for a guessing the output file, please specify it with -o");
                    print_help(file);
                }
            }
        },
        inputs,
        program_type,
    }
}

fn execute(command: &mut Command) -> anyhow::Result<()> {
    let formatted_command = format!(
        "could not spawn command {} with args [{}]",
        command.get_program().to_str().unwrap_or("<invalid utf8>"),
        command
            .get_args()
            .map(|e| e.to_str().unwrap_or("<invalid utf8>"))
            .collect::<Vec<_>>()
            .join(", "),
    );
    let mut command = command.spawn().context(formatted_command.clone())?;
    let exit_code = command
        .wait()
        .context(format!("failed to wait for {}", &formatted_command))?;

    if !exit_code.success() {
        bail!(
            "command {} failed with exit code {}\nstdout:{:?}\nstderr:{:?}",
            &formatted_command,
            exit_code,
            command.stdout,
            command.stdin,
        );
    }

    Ok(())
}

fn preprocess(input: &PathBuf, output: &PathBuf) -> anyhow::Result<()> {
    execute(
        Command::new("gcc")
            .arg("-E")
            .arg("-P")
            .arg(input)
            .arg("-o")
            .arg(&output),
    )?;

    if !output.exists() {
        bail!("the preprocessor exited sucessfully without leaving any output file");
    }

    Ok(())
}

fn assemble(program_type: ProgramType, input: &[PathBuf], output: &PathBuf) -> anyhow::Result<()> {
    let mut to_run = Command::new("gcc");
    to_run.args(input).arg("-o").arg(output);

    if let ProgramType::Object = program_type {
        to_run.arg("-c");
    }

    execute(&mut to_run)?;
    if !output.exists() {
        bail!("the assembler exited sucessfully without leaving any output file");
    }
    Ok(())
}

fn lex(input: PathBuf) -> anyhow::Result<()> {
    let input = fs::read_to_string(&input).context(format!(
        "failed to read file {}",
        input.as_os_str().to_str().unwrap_or("<invalid utf8>")
    ))?;

    let lexer = rrbc_parser::lexer::Lexer::new(input);

    for tok in lexer {
        tok?;
    }

    Ok(())
}

fn parse(input: &PathBuf) -> anyhow::Result<rrbc_parser::ast::Program> {
    let input = fs::read_to_string(input).context(format!(
        "failed to read file {}",
        input.as_os_str().to_str().unwrap_or("<invalid utf8>")
    ))?;

    let lexer = rrbc_parser::lexer::Lexer::new(input);

    let mut parser = rrbc_parser::Parser::try_build(lexer).context("failed to build parser")?;
    parser
        .parse_program()
        .context("failed to parse the program")
}

#[cfg(feature = "validate")]
fn validate(
    program: rrbc_parser::ast::Program,
) -> anyhow::Result<(
    rrbc_parser::ast::Program,
    rrbc_semantic_analysis::type_checking::Symbols,
)> {
    use rrbc_semantic_analysis::{
        identifier_resolution, label_resolution, loop_labeling, switch_resolution, type_checking,
    };

    let program =
        identifier_resolution::resolve_program(program).context("identifier resolution failed")?;

    let (program, symbols) =
        type_checking::typecheck_program(program).context("type checking failed")?;
    let program = label_resolution::resolve_program(program).context("label resoultion failed")?;
    let program = loop_labeling::label_program(program).context("loop labeling failed")?;
    let program =
        switch_resolution::resolve_program(program).context("switch resolution failed")?;

    Ok((program, symbols))
}
#[cfg(feature = "tackygen")]
fn tackygen(program: rrbc_parser::ast::Program) -> rrbc_tacky::Program {
    rrbc_tackygen::emit_tacky_program(program)
}

#[cfg(feature = "asmgen")]
fn asmgen(program: rrbc_tacky::Program) -> rrbc_asm::Program {
    rrbc_asmgen::code_generation(program)
}

#[cfg(feature = "emit")]
fn emit(
    program: rrbc_asm::Program,
    symbols: rrbc_semantic_analysis::type_checking::Symbols,
    to: PathBuf,
) -> anyhow::Result<()> {
    fs::write(to, program.emit(0, &symbols))?;

    Ok(())
}

pub fn run() -> anyhow::Result<()> {
    let args = parse_program_args(env::args());
    if args.debug {
        println!("Configuration {args:#?}");
    }
    check_feature_flags_with_goal(&args.goal);

    #[allow(unused_mut)]
    let mut assembly_output_files = vec![];
    _ = assembly_output_files;

    for input in args.inputs {
        let pp_file = input.with_extension("i");
        let asm_file = input.with_extension("S");
        _ = asm_file;
        preprocess(&input, &pp_file)?;

        if let Goal::Lex = args.goal {
            lex(pp_file)?;
            continue;
        }

        let program = parse(&pp_file)?;

        if let Err(err) = fs::remove_file(&pp_file) {
            eprintln!(
                "WARN: Could not remove the file {:?} due to {}",
                &pp_file, err
            )
        }

        if let Goal::Parse = args.goal {
            println!(
                "{}: {:#?}",
                pp_file.to_str().unwrap_or("<invalid utf8>"),
                program
            );
            continue;
        }

        #[cfg(feature = "validate")]
        {
            let (program, symbols) = validate(program)?;
            _ = program;
            _ = symbols;

            if let Goal::Validate = args.goal {
                continue;
            }

            #[cfg(feature = "tackygen")]
            {
                let program = tackygen(program);
                _ = program;

                if let Goal::Tacky = args.goal {
                    println!(
                        "{}: {:#?}",
                        pp_file.to_str().unwrap_or("<invalid utf8>"),
                        program
                    );
                    continue;
                }

                #[cfg(feature = "asmgen")]
                {
                    let program = asmgen(program);

                    if let Goal::Codegen = args.goal {
                        println!(
                            "{}: {:#?}",
                            pp_file.to_str().unwrap_or("<invalid utf8>"),
                            program
                        );
                        continue;
                    }

                    #[cfg(feature = "emit")]
                    {
                        emit(program, symbols, asm_file.clone())?;
                        assembly_output_files.push(asm_file);
                    }
                }
            }
        }
    }

    if let Goal::Compile = args.goal {
        assemble(args.program_type, &assembly_output_files, &args.output)?;
        for file in assembly_output_files {
            if let Err(err) = fs::remove_file(&file) {
                eprintln!("WARN: Could not remove the file {:?} due to {}", &file, err)
            }
        }
    }

    Ok(())
}
