use crate::{
    ast::{self, BinaryOperator},
    tacky::{FunctionDefiniton, Instruction, Program, Value, Var},
    unique_id,
};

pub fn emit_tacky_expression(expr: ast::Expression) -> (Vec<Instruction>, Value) {
    match expr {
        ast::Expression::Constant(val) => (vec![], Value::Constant(val)),
        ast::Expression::Unary(op, expr) => {
            let (mut inner_ins, val) = emit_tacky_expression(*expr);
            let dst_name = unique_id::temp_variable_name();
            let dst = Value::Var(Var(dst_name.clone()));
            inner_ins.append(&mut vec![Instruction::Unary {
                operator: op.to_tacky(),
                src: val,
                dst: Var(dst_name.clone()),
            }]);

            (inner_ins, dst)
        }
        ast::Expression::Binary {
            op: BinaryOperator::Or,
            lhs,
            rhs,
        } => {
            let (mut instructions, lhs) = emit_tacky_expression(*lhs);
            let true_label = unique_id::temp_lable_name("or_false");
            let end_label = unique_id::temp_lable_name("or_end");
            let dst_name = unique_id::temp_variable_name();
            let dst = Value::Var(Var(dst_name.clone()));
            instructions.push(Instruction::JumpIfNotZero(lhs, true_label.clone()));
            let (mut instructions2, rhs) = emit_tacky_expression(*rhs);
            instructions.append(&mut instructions2);
            instructions.push(Instruction::JumpIfNotZero(rhs, true_label.clone()));
            instructions.append(&mut vec![
                Instruction::Copy(Value::Constant(0), Value::Var(Var(dst_name.clone()))),
                Instruction::Jump(end_label.clone()),
                Instruction::Label(true_label),
                Instruction::Copy(Value::Constant(1), Value::Var(Var(dst_name.clone()))),
                Instruction::Label(end_label),
            ]);
            (instructions, dst)
        }
        ast::Expression::Binary {
            op: BinaryOperator::And,
            lhs,
            rhs,
        } => {
            let (mut instructions, lhs) = emit_tacky_expression(*lhs);
            let false_label = unique_id::temp_lable_name("and_false");
            let end_label = unique_id::temp_lable_name("and_end");
            let dst_name = unique_id::temp_variable_name();
            let dst = Value::Var(Var(dst_name.clone()));
            instructions.push(Instruction::JumpIfZero(lhs, false_label.clone()));
            let (mut instructions2, rhs) = emit_tacky_expression(*rhs);
            instructions.append(&mut instructions2);
            instructions.push(Instruction::JumpIfZero(rhs, false_label.clone()));
            instructions.append(&mut vec![
                Instruction::Copy(Value::Constant(1), Value::Var(Var(dst_name.clone()))),
                Instruction::Jump(end_label.clone()),
                Instruction::Label(false_label),
                Instruction::Copy(Value::Constant(0), Value::Var(Var(dst_name.clone()))),
                Instruction::Label(end_label),
            ]);
            (instructions, dst)
        }
        ast::Expression::Binary { op, lhs, rhs } => {
            let (mut instructions, lhs) = emit_tacky_expression(*lhs);
            let mut rhs_expr = emit_tacky_expression(*rhs);
            instructions.append(&mut rhs_expr.0);
            let dst_name = unique_id::temp_variable_name();
            let dst = Value::Var(Var(dst_name.clone()));
            instructions.append(&mut vec![Instruction::Binary {
                op: op.to_tacky(),
                lhs,
                rhs: rhs_expr.1,
                dst: Var(dst_name),
            }]);
            (instructions, dst)
        }
        ast::Expression::Var(v) => (vec![], Value::Var(Var(v))),
        ast::Expression::Assignment { lhs, rhs } => match *lhs {
            ast::Expression::Var(v) => {
                let (mut instructions, value) = emit_tacky_expression(*rhs);
                instructions.append(&mut vec![Instruction::Copy(
                    value,
                    Value::Var(Var(v.clone())),
                )]);
                (instructions, Value::Var(Var(v)))
            }
            _ => unreachable!("Ran into an assignment with invalid lhs"),
        },
    }
}

pub fn emit_tacky_statement(stmt: ast::Statement) -> Vec<Instruction> {
    match stmt {
        ast::Statement::Return(expr) => {
            let (mut inners_ins, v) = emit_tacky_expression(expr);
            inners_ins.push(Instruction::Return(v));
            inners_ins
        }
        ast::Statement::Expression(expression) => emit_tacky_expression(expression).0,
        ast::Statement::Null => vec![],
    }
}

pub fn emit_tacky_declaration(declaration: ast::Declaration) -> Vec<Instruction> {
    if let Some(expr) = declaration.exp {
        let (mut instructions, value) = emit_tacky_expression(expr);
        instructions.append(&mut vec![Instruction::Copy(
            value,
            Value::Var(Var(declaration.name)),
        )]);
        instructions
    } else {
        vec![]
    }
}

pub fn emit_tacky_block_item(block_item: ast::BlockItem) -> Vec<Instruction> {
    match block_item {
        ast::BlockItem::S(statement) => emit_tacky_statement(statement),
        ast::BlockItem::D(declaration) => emit_tacky_declaration(declaration),
    }
}

pub fn emit_tacky_function(func: ast::FunctionDefinition) -> FunctionDefiniton {
    let mut body = vec![];

    for stmt in func.body {
        body.append(&mut emit_tacky_block_item(stmt));
    }

    body.append(&mut vec![Instruction::Return(Value::Constant(0))]);

    FunctionDefiniton {
        identifier: func.name,
        body,
    }
}

pub fn emit_tacky_program(program: ast::Program) -> Program {
    Program(emit_tacky_function(program.function_definition))
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn basic_binary() {
        let lexer = Lexer::new(
            r"
        int main(void) {
            return 1 + 1;
        }
        "
            .to_owned(),
        );
        let mut parser = Parser::try_build(lexer).expect("parser should be created successfully");

        let program = parser
            .parse_program()
            .expect("the program should be parsed successfully");

        let program = emit_tacky_program(program);

        let expected = [
            Instruction::Binary {
                op: crate::tacky::BinaryOperator::Add,
                lhs: Value::Constant(1),
                rhs: Value::Constant(1),
                dst: Var("tmp.0".to_owned()),
            },
            Instruction::Return(Value::Var(Var("tmp.0".to_owned()))),
        ];

        assert_eq!(expected.len(), program.0.body.len());

        for pair in expected.iter().zip(program.0.body.iter()) {
            assert_eq!(pair.0, pair.1);
        }
    }
}
