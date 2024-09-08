use crate::assembly::{FunctionDefinition, Instruction, Operand, Program};

pub fn run_third_pass(program: Program, stack_offset: u32) -> Program {
    Program(function(program.0, stack_offset))
}

fn function(func: FunctionDefinition, stack_offset: u32) -> FunctionDefinition {
    let mut new_instructions = vec![Instruction::AllocateStack(stack_offset)];

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
            _ => {
                new_instructions.push(inst);
            }
        }
    }

    new_instructions
}
