use crate::assembly::{
    BinaryOperator, FunctionDefinition, Instruction, Operand, Program, Register,
};

pub fn run_third_pass(program: Program, stack_offset: u32) -> Program {
    Program(function(program.0, stack_offset))
}

fn function(func: FunctionDefinition, stack_offset: u32) -> FunctionDefinition {
    let mut new_instructions = vec![];

    if stack_offset > 0 {
        new_instructions.push(Instruction::AllocateStack(stack_offset));
    }

    new_instructions.append(&mut instructions(func.instructions));

    FunctionDefinition {
        name: func.name,
        instructions: new_instructions,
    }
}

fn instructions(instructions: Vec<Instruction>) -> Vec<Instruction> {
    let mut new_instructions = vec![];

    for inst in instructions {
        match inst {
            Instruction::Mov { src, dst } => match (src, dst) {
                (Operand::Stack(val1), Operand::Stack(val2)) => {
                    new_instructions.push(Instruction::Mov {
                        src: Operand::Stack(val1),
                        dst: Operand::Register(crate::assembly::Register::R10),
                    });
                    new_instructions.push(Instruction::Mov {
                        src: Operand::Register(crate::assembly::Register::R10),
                        dst: Operand::Stack(val2),
                    });
                }
                (src, dst) => new_instructions.push(Instruction::Mov { src, dst }),
            },
            Instruction::Binary(BinaryOperator::Mult, lhs, Operand::Stack(rhs)) => {
                new_instructions.push(Instruction::Mov {
                    src: Operand::Stack(rhs),
                    dst: Operand::Register(Register::R11),
                });
                new_instructions.push(Instruction::Binary(
                    BinaryOperator::Mult,
                    lhs,
                    Operand::Register(Register::R11),
                ));
                new_instructions.push(Instruction::Mov {
                    src: Operand::Register(Register::R11),
                    dst: Operand::Stack(rhs),
                });
            }
            Instruction::Binary(op, Operand::Stack(lhs), Operand::Stack(rhs)) => {
                new_instructions.push(Instruction::Mov {
                    src: Operand::Stack(lhs),
                    dst: Operand::Register(Register::R10),
                });
                new_instructions.push(Instruction::Binary(
                    op,
                    Operand::Register(Register::R10),
                    Operand::Stack(rhs),
                ));
            }
            Instruction::Idiv(Operand::Imm(val)) => {
                new_instructions.push(Instruction::Mov {
                    src: Operand::Imm(val),
                    dst: Operand::Register(Register::R10),
                });
                new_instructions.push(Instruction::Idiv(Operand::Register(Register::R10)));
            }
            _ => {
                new_instructions.push(inst);
            }
        }
    }

    new_instructions
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_multiply() {
        let input_instructions = vec![Instruction::Binary(
            BinaryOperator::Mult,
            Operand::Stack(4),
            Operand::Stack(8),
        )];
        let expected_instructions = vec![
            Instruction::Mov {
                src: Operand::Stack(8),
                dst: Operand::Register(Register::R11),
            },
            Instruction::Binary(
                BinaryOperator::Mult,
                Operand::Stack(4),
                Operand::Register(Register::R11),
            ),
            Instruction::Mov {
                src: Operand::Register(Register::R11),
                dst: Operand::Stack(8),
            },
        ];
        let output = instructions(input_instructions);

        assert_eq!(expected_instructions.len(), output.len());

        for (expected, received) in expected_instructions.into_iter().zip(output) {
            assert_eq!(expected, received);
        }
    }
}
