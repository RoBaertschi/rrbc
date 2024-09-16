use std::env::consts::OS;

use crate::assembly::{
    BinaryOperator, CondCode, FunctionDefinition, Instruction, Operand, Program, Register,
    RegisterSize, UnaryOperator,
};

/// A Structure that implements this trait, can emit assembly using the provided function.
/// A high-level construct should also add a comment with more debugging information.
/// A high-level construct would currently be a function.
pub trait EmitAsm {
    /// The indent_depth argument only needs to be used when you have to indent something.
    fn emit(&self, indent_depth: u32) -> String;
}

impl Operand {
    fn emit(&self) -> String {
        self.emit_size(RegisterSize::All)
    }
    fn emit_size(&self, size: RegisterSize) -> String {
        match self {
            Operand::Register(reg) => {
                "%".to_owned()
                    + match reg {
                        crate::assembly::Register::AX => match size {
                                RegisterSize::All => "eax",
                                RegisterSize::Lower => "al",
                            },
                        crate::assembly::Register::R10 => match size { RegisterSize::All => "r10d", RegisterSize::Lower => "r10d"},
                        crate::assembly::Register::DX => match size {
                            RegisterSize::All => "edx",
                            RegisterSize::Lower => "al"
                        },
                        crate::assembly::Register::R11 => match size {
                            RegisterSize::All => "r11d",
                            RegisterSize::Lower => "r11b"
                        },
                        crate::assembly::Register::CX => match size {
                            RegisterSize::All => "ecx",
                            RegisterSize::Lower => "cl",
                        },
                    }
            }
            Operand::Imm(val) => format!("${}", val),
            Operand::Pseudo(_) => unreachable!("got a unexpected Operand::Pseudo, this should not happen and indicates a bug in the third codegen pass"),
            Operand::Stack(stack) => format!("{}(%rbp)", -(*stack as i64)),
        }
    }
}

impl EmitAsm for Instruction {
    fn emit(&self, indent_depth: u32) -> String {
        let tabs = "\t".repeat(indent_depth as usize);

        match self {
            Instruction::Mov { src, dst } => {
                format!("{}movl {}, {}\n", tabs, src.emit(), dst.emit(),)
            }
            Instruction::Ret => {
                format!("{}movq %rbp, %rsp\n{}popq %rbp\n{}ret\n", tabs, tabs, tabs)
            }
            Instruction::Unary { op, operand } => {
                format!("{}{} {}\n", tabs, op.emit(indent_depth), operand.emit())
            }
            Instruction::AllocateStack(val) => format!("{}subq ${}, %rsp\n", tabs, *val),

            Instruction::Binary {
                op: op @ BinaryOperator::Sal | op @ BinaryOperator::Sar,
                lhs,
                rhs,
            } => format!(
                "{}{} {},{}\n",
                tabs,
                op.emit(indent_depth),
                lhs.emit(),
                match rhs {
                    Operand::Register(Register::CX) => rhs.emit_size(RegisterSize::Lower),
                    rhs => rhs.emit(),
                },
            ),

            Instruction::Binary { op, lhs, rhs } => format!(
                "{}{} {},{}\n",
                tabs,
                op.emit(indent_depth),
                lhs.emit(),
                rhs.emit(),
            ),
            Instruction::Idiv(op) => format!("{}idivl {}\n", tabs, op.emit()),
            Instruction::Cdq => "cdq\n".to_owned(),
            Instruction::Cmp { lhs, rhs } => {
                format!("{}cmpl {},{}\n", tabs, lhs.emit(), rhs.emit(),)
            }
            Instruction::Jmp(label) => format!("{}jmp .L{}\n", tabs, label),
            Instruction::JumpCC(cond_code, label) => {
                format!("{}j{} .L{}\n", tabs, cond_code.emit(indent_depth), label)
            }
            Instruction::SetCC(cond_code, operand) => format!(
                "{}set{} {}\n",
                tabs,
                cond_code.emit(indent_depth),
                operand.emit_size(RegisterSize::Lower)
            ),
            Instruction::Label(label) => format!(".L{}:\n", label),
        }
    }
}

impl EmitAsm for CondCode {
    fn emit(&self, _: u32) -> String {
        match self {
            CondCode::E => "e",
            CondCode::NE => "ne",
            CondCode::G => "g",
            CondCode::GE => "ge",
            CondCode::L => "l",
            CondCode::LE => "le",
        }
        .to_owned()
    }
}

impl EmitAsm for UnaryOperator {
    fn emit(&self, _: u32) -> String {
        match self {
            UnaryOperator::Neg => "negl".to_owned(),
            UnaryOperator::Not => "notl".to_owned(),
        }
    }
}

impl EmitAsm for BinaryOperator {
    fn emit(&self, _: u32) -> String {
        match self {
            BinaryOperator::Add => "addl",
            BinaryOperator::Sub => "subl",
            BinaryOperator::Mult => "imull",
            BinaryOperator::Sal => "sall",
            BinaryOperator::Sar => "sarl",
            BinaryOperator::And => "andl",
            BinaryOperator::Xor => "xorl",
            BinaryOperator::Or => "orl",
        }
        .to_owned()
    }
}

impl EmitAsm for FunctionDefinition {
    fn emit(&self, indent_depth: u32) -> String {
        let tabs = "\t".repeat((indent_depth + 1) as usize);

        let start = format!("{}pushq %rbp\n{}movq %rsp, %rbp", tabs, tabs);

        format!(
            "# {:?}\n{}\n{}:\n{}\n{}\n# End function\n",
            self,
            format!("{}.globl {}", tabs, self.name),
            if OS == "macos" {
                format!("_{}", self.name)
            } else {
                self.name.to_owned()
            },
            start,
            self.instructions
                .iter()
                .map(|inst| inst.emit(indent_depth + 1))
                .reduce(|acc, inst| format!("{}{}", acc, inst))
                .unwrap_or("".to_owned()),
        )
    }
}

impl EmitAsm for Program {
    fn emit(&self, indent_depth: u32) -> String {
        format!(
            "{}\n{}",
            self.0.emit(indent_depth),
            if OS == "linux" {
                ".section .note.GNU-stack,\"\",@progbits\n"
            } else {
                ""
            }
        )
    }
}
