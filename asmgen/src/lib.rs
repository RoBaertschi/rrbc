use rrbc_asm as assembly;
use rrbc_tacky as tacky;

pub mod fixup_instructions;
pub mod replace_pseudo;
pub mod tacky_to_assembly;

pub fn code_generation(program: tacky::Program) -> assembly::Program {
    let program = tacky_to_assembly::code_generation(program);
    let (program, stack_offset) = replace_pseudo::run_second_pass(program);

    fixup_instructions::run_third_pass(program, stack_offset)
}

#[cfg(test)]
mod tests {
    use assembly::Instruction;

    use rrbc_parser::lexer::Lexer;
    use rrbc_parser::Parser;

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

        let program = rrbc_tackygen::emit_tacky_program(program);

        let program = code_generation(program);

        assert_eq!(program.0[0].name, "main");
        assert_eq!(
            program.0[0].instructions,
            vec![
                Instruction::Mov {
                    src: assembly::Operand::Imm(2),
                    dst: assembly::Operand::Register(assembly::Register::AX)
                },
                Instruction::Ret,
                Instruction::Mov {
                    src: assembly::Operand::Imm(0),
                    dst: assembly::Operand::Register(assembly::Register::AX)
                },
                Instruction::Ret,
            ]
        )
    }
}
