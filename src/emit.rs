use std::env::consts::OS;

use crate::{
    assembly::{
        BinaryOperator, CondCode, FunctionDefinition, Instruction, Operand, Program, Register,
        RegisterSize, UnaryOperator,
    },
    semantic_analysis::type_checking::Symbols,
};

/// A Structure that implements this trait, can emit assembly using the provided function.
/// A high-level construct should also add a comment with more debugging information.
/// A high-level construct would currently be a function.
pub trait EmitAsm {
    /// The indent_depth argument only needs to be used when you have to indent something.
    fn emit(&self, indent_depth: u32, symbols: &Symbols) -> String;
}

impl Operand {
    fn emit(&self) -> String {
        self.emit_size(RegisterSize::Four)
    }
    fn emit_size(&self, size: RegisterSize) -> String {
        match self {
            Operand::Register(reg) =>
                format!("%{}", match reg {
                        Register::AX => match size {
                            RegisterSize::Eight => "rax",
                            RegisterSize::Four => "eax",
                            RegisterSize::One => "al",
                        },
                        Register::DX => match size {
                            RegisterSize::Eight => "rdx",
                            RegisterSize::Four => "edx",
                            RegisterSize::One => "dl",
                        },
                        Register::CX => match size {
                            RegisterSize::Eight => "rcx",
                            RegisterSize::Four => "ecx",
                            RegisterSize::One => "cl",
                        },
                        Register::DI => match size {
                            RegisterSize::One => "dil",
                            RegisterSize::Four => "edi",
                            RegisterSize::Eight => "rdi",
                        },
                        Register::SI => match size {
                            RegisterSize::One => "sil",
                            RegisterSize::Four => "esi",
                            RegisterSize::Eight => "rsi",
                        },
                        Register::R8 => match size {
                            RegisterSize::Eight => "r8",
                            RegisterSize::Four => "r8d",
                            RegisterSize::One => "r8b",
                        },
                        Register::R9 => match size {
                            RegisterSize::Eight => "r9",
                            RegisterSize::Four => "r9d",
                            RegisterSize::One => "r9b",
                        },
                        Register::R10 => match size {
                            RegisterSize::Eight => "r10",
                            RegisterSize::Four => "r10d",
                            RegisterSize::One => "r10b",
                        },
                        Register::R11 => match size {
                            RegisterSize::Eight => "r11",
                            RegisterSize::Four => "r11d",
                            RegisterSize::One => "r11b",
                        },
                    }),
            Operand::Imm(val) => format!("${}", val),
            Operand::Pseudo(_) => unreachable!("got a unexpected Operand::Pseudo, this should not happen and indicates a bug in the third codegen pass"),
            Operand::Stack(stack) => format!("{}(%rbp)", *stack ),
        }
    }
}

impl EmitAsm for Instruction {
    fn emit(&self, indent_depth: u32, symbols: &Symbols) -> String {
        let tabs = "\t".repeat(indent_depth as usize);

        match self {
            Instruction::Mov { src, dst } => {
                format!("{}movl {}, {}\n", tabs, src.emit(), dst.emit(),)
            }
            Instruction::Ret => {
                format!("{}movq %rbp, %rsp\n{}popq %rbp\n{}ret\n", tabs, tabs, tabs)
            }
            Instruction::Unary { op, operand } => {
                format!(
                    "{}{} {}\n",
                    tabs,
                    op.emit(indent_depth, symbols),
                    operand.emit()
                )
            }
            Instruction::AllocateStack(val) => format!("{}subq ${}, %rsp\n", tabs, *val),

            Instruction::Binary {
                op: op @ (BinaryOperator::Sal | BinaryOperator::Sar),
                lhs,
                rhs,
            } => format!(
                "{}{} {},{}\n",
                tabs,
                op.emit(indent_depth, symbols),
                lhs.emit(),
                match rhs {
                    Operand::Register(Register::CX) => rhs.emit_size(RegisterSize::One),
                    rhs => rhs.emit(),
                },
            ),

            Instruction::Binary { op, lhs, rhs } => format!(
                "{}{} {},{}\n",
                tabs,
                op.emit(indent_depth, symbols),
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
                format!(
                    "{}j{} .L{}\n",
                    tabs,
                    cond_code.emit(indent_depth, symbols),
                    label
                )
            }
            Instruction::SetCC(cond_code, operand) => format!(
                "{}set{} {}\n",
                tabs,
                cond_code.emit(indent_depth, symbols),
                operand.emit_size(RegisterSize::One)
            ),
            Instruction::Label(label) => format!(".L{}:\n", label),
            Instruction::DeallocateStack(amount) => format!("{}addq ${}, %rsp\n", tabs, *amount),
            Instruction::Push(operand) => {
                format!("{}pushq {}\n", tabs, operand.emit_size(RegisterSize::Eight))
            }
            Instruction::Call(ident) => {
                format!(
                    "{}call {ident}{}\n",
                    tabs,
                    if OS == "linux" {
                        if symbols.get(ident).map(|sym| sym.1).unwrap_or(false) {
                            ""
                        } else {
                            "@PLT"
                        }
                    } else {
                        ""
                    }
                )
            }
        }
    }
}

impl EmitAsm for CondCode {
    fn emit(&self, _: u32, _: &Symbols) -> String {
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
    fn emit(&self, _: u32, _: &Symbols) -> String {
        match self {
            UnaryOperator::Neg => "negl".to_owned(),
            UnaryOperator::Not => "notl".to_owned(),
        }
    }
}

impl EmitAsm for BinaryOperator {
    fn emit(&self, _: u32, _: &Symbols) -> String {
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
    fn emit(&self, indent_depth: u32, symbols: &Symbols) -> String {
        let tabs = "\t".repeat((indent_depth + 1) as usize);

        let start = format!("{}pushq %rbp\n{}movq %rsp, %rbp", tabs, tabs);

        format!(
            "# {:?}\n{}.globl {}\n{}:\n{}\n{}\n# End function\n",
            self,
            tabs,
            self.name,
            if OS == "macos" {
                format!("_{}", self.name)
            } else {
                self.name.to_owned()
            },
            start,
            self.instructions
                .iter()
                .map(|inst| inst.emit(indent_depth + 1, symbols))
                .reduce(|acc, inst| format!("{}{}", acc, inst))
                .unwrap_or("".to_owned()),
        )
    }
}

impl EmitAsm for Program {
    fn emit(&self, indent_depth: u32, symbols: &Symbols) -> String {
        format!(
            "{}\n{}",
            self.0
                .iter()
                .map(|func| func.emit(indent_depth, symbols))
                .reduce(|acm, item| format!("{}\n{}", acm, item))
                .unwrap_or(String::new()),
            if OS == "linux" {
                ".section .note.GNU-stack,\"\",@progbits\n"
            } else {
                ""
            }
        )
    }
}
