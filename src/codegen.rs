use crate::{assembly, ast};

pub fn code_generation(program: ast::Program) -> assembly::Program {
    let instructions = cg_statement(program.function_definition.body);

    assembly::Program(assembly::FunctionDefinition {
        name: program.function_definition.name,
        instructions,
    })
}

fn cg_statement(stat: ast::Statement) -> Vec<assembly::Instruction> {
    match stat {
        ast::Statement::Return(expr) => cg_return_stat(expr),
    }
}

fn cg_return_stat(expr: ast::Expression) -> Vec<assembly::Instruction> {
    let mut instructions = cg_expression(expr, assembly::Register::EAX);

    instructions.push(assembly::Instruction::Ret());

    instructions
}

fn cg_expression(
    expr: ast::Expression,
    register: assembly::Register,
) -> Vec<assembly::Instruction> {
    match expr {
        ast::Expression::Constant(val) => vec![assembly::Instruction::Mov {
            src: assembly::Operand::Imm(val),
            dst: assembly::Operand::Register(register),
        }],
    }
}

#[cfg(test)]
mod tests {
    use assembly::Instruction;

    use crate::{lexer::Lexer, parser::Parser};

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

        let program = code_generation(program);

        assert_eq!(program.0.name, "main");
        assert_eq!(
            program.0.instructions,
            vec![
                Instruction::Mov {
                    src: assembly::Operand::Imm(2),
                    dst: assembly::Operand::Register(assembly::Register::EAX)
                },
                Instruction::Ret(),
            ]
        )
    }
}
