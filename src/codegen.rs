use crate::{assembly, ast, tacky};

pub mod pass1;
pub mod pass2;
pub mod pass3;

pub fn code_generation(program: tacky::Program) -> assembly::Program {
    let program = pass1::code_generation(program);
    let (program, stack_offset) = pass2::run_second_pass(program);
    let program = pass3::run_third_pass(program, stack_offset);
    program
}

#[cfg(test)]
mod tests {
    use assembly::Instruction;

    use crate::{lexer::Lexer, parser::Parser, tackler};

    use super::*;

    #[test]
    fn test_parse_function() {
        let lexer = Lexer::new(
            r"
        int main(void) {
            return 2;
        }
        "
            .to_owned(),
        );
        let mut parser = Parser::try_build(lexer).expect("parser should be created successfully");

        let program = parser
            .parse_program()
            .expect("the program should be parsed successfully");

        let program = tackler::emit_tacky_program(program);

        let program = code_generation(program);

        assert_eq!(program.0.name, "main");
        assert_eq!(
            program.0.instructions,
            vec![
                Instruction::Mov {
                    src: assembly::Operand::Imm(2),
                    dst: assembly::Operand::Register(assembly::Register::AX)
                },
                Instruction::Ret,
            ]
        )
    }
}
