pub mod fixup_instructions;
pub mod replace_pseudo;
pub mod tacky_to_assembly;

pub fn code_generation(program: rrbc_tacky::Program) -> rrbc_asm::Program {
    let program = tacky_to_assembly::code_generation(program);
    let (program, stack_offset) = replace_pseudo::replace_pseudo_program(program);

    fixup_instructions::fixup_program(program, stack_offset)
}

#[cfg(test)]
mod tests {
    use rrbc_asm::Instruction;

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
                    src: rrbc_asm::Operand::Imm(2),
                    dst: rrbc_asm::Operand::Register(rrbc_asm::Register::AX)
                },
                Instruction::Ret,
                Instruction::Mov {
                    src: rrbc_asm::Operand::Imm(0),
                    dst: rrbc_asm::Operand::Register(rrbc_asm::Register::AX)
                },
                Instruction::Ret,
            ]
        )
    }
}
