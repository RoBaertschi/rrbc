use std::collections::HashMap;

use rrbc_asm::{self, FunctionDefinition, Instruction, Program};

pub fn replace_pseudo_program(program: Program) -> (Program, HashMap<String, i64>) {
    ReplacePseudoPass {
        offest_to_identifier: HashMap::new(),
        stack_offset: HashMap::new(),
        current_function: String::new(),
    }
    .program(program)
}

struct ReplacePseudoPass {
    offest_to_identifier: HashMap<String, i64>,
    stack_offset: HashMap<String, i64>,
    current_function: String,
}

impl ReplacePseudoPass {
    pub fn program(mut self, program: rrbc_asm::Program) -> (Program, HashMap<String, i64>) {
        (
            Program(
                program
                    .0
                    .into_iter()
                    .map(|func| self.function(func))
                    .collect(),
            ),
            self.stack_offset,
        )
    }

    fn function(&mut self, func: rrbc_asm::FunctionDefinition) -> rrbc_asm::FunctionDefinition {
        self.current_function = func.name.clone();
        FunctionDefinition {
            name: func.name,
            instructions: self.instructions(func.instructions),
        }
    }

    fn instructions(
        &mut self,
        mut instructions: Vec<rrbc_asm::Instruction>,
    ) -> Vec<rrbc_asm::Instruction> {
        for inst in instructions.iter_mut() {
            match inst {
                Instruction::Ret => {}
                Instruction::Mov { src, dst } => {
                    self.pseudo_to_stack(src);
                    self.pseudo_to_stack(dst);
                }
                Instruction::Unary { op: _, operand } => self.pseudo_to_stack(operand),
                Instruction::Binary { op: _, lhs, rhs } => {
                    self.pseudo_to_stack(lhs);
                    self.pseudo_to_stack(rhs);
                }
                Instruction::Idiv(operand) => self.pseudo_to_stack(operand),
                Instruction::Push(operand) => self.pseudo_to_stack(operand),
                Instruction::Cdq
                | Instruction::AllocateStack(_)
                | Instruction::Jmp(_)
                | Instruction::JumpCC(_, _)
                | Instruction::Label(_)
                | Instruction::DeallocateStack(_)
                | Instruction::Call(_) => {}
                Instruction::Cmp { lhs, rhs } => {
                    self.pseudo_to_stack(lhs);
                    self.pseudo_to_stack(rhs);
                }
                Instruction::SetCC(_, operand) => {
                    self.pseudo_to_stack(operand);
                }
            }
        }

        instructions
    }

    fn pseudo_to_stack(&mut self, operand: &mut rrbc_asm::Operand) {
        if let rrbc_asm::Operand::Pseudo(ident) = operand {
            *operand = rrbc_asm::Operand::Stack(
                *self
                    .offest_to_identifier
                    .entry(ident.clone())
                    .or_insert_with(|| {
                        let stack_offset = self
                            .stack_offset
                            .entry(self.current_function.clone())
                            .or_insert(0);
                        *stack_offset -= 4;
                        *stack_offset
                    }),
            )
        }
    }
}
