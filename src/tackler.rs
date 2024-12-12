use crate::{
    ast::{self, BinaryOperator},
    tacky::{self, FunctionDefiniton, Instruction, Program, Value, Var},
    unique_id,
};

pub fn emit_tacky_expression(expr: ast::Expression) -> (Vec<Instruction>, Value) {
    match expr {
        ast::Expression::Constant(val) => (vec![], Value::Constant(val)),
        ast::Expression::Binary {
            op: BinaryOperator::Or,
            lhs,
            rhs,
        } => {
            let (mut instructions, lhs) = emit_tacky_expression(*lhs);
            let true_label = unique_id::temp_label_name("or_false");
            let end_label = unique_id::temp_label_name("or_end");
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
            let false_label = unique_id::temp_label_name("and_false");
            let end_label = unique_id::temp_label_name("and_end");
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
        // WARN: lhs should only be evaluated once!
        ast::Expression::Assignment { op, lhs, rhs } => match *lhs {
            ast::Expression::Var(v) => {
                let (mut instructions, value) = emit_tacky_expression(*rhs);
                match op {
                    ast::AssignmentOperator::None => {
                        instructions.append(&mut vec![Instruction::Copy(
                            value,
                            Value::Var(Var(v.clone())),
                        )]);
                    }
                    op => {
                        instructions.append(&mut vec![Instruction::Binary {
                            op: op.to_tacky(),
                            lhs: Value::Var(Var(v.clone())),
                            rhs: value,
                            dst: Var(v.clone()),
                        }]);
                    }
                }
                (instructions, Value::Var(Var(v)))
            }
            _ => unreachable!("Ran into an assignment with invalid lhs lvalue"),
        },
        // Increment and decrement
        // WARN: expression should only be evaluated once!
        ast::Expression::Unary {
            op: op @ (ast::UnaryOperator::Increment | ast::UnaryOperator::Decrement),
            expression,
        } => {
            let binary_op = match op {
                ast::UnaryOperator::Increment => tacky::BinaryOperator::Add,
                ast::UnaryOperator::Decrement => tacky::BinaryOperator::Subtract,
                _ => unreachable!(),
            };
            let (mut instructions, value) = emit_tacky_expression(*expression);

            match value {
                Value::Var(var) => {
                    instructions.append(&mut vec![Instruction::Binary {
                        op: binary_op,
                        lhs: Value::Var(var.clone()),
                        rhs: Value::Constant(1),
                        dst: var.clone(),
                    }]);

                    (instructions, Value::Var(var))
                }
                _ => unreachable!("Ran into an assignment with invalid lhs lvalue"),
            }
        }
        ast::Expression::Unary {
            op,
            expression: expr,
        } => {
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
        // WARN: expression should only be evaluated once!
        ast::Expression::Postfix(postfix_operator, expression) => {
            let binary_op = match postfix_operator {
                ast::PostfixOperator::Increment => tacky::BinaryOperator::Add,
                ast::PostfixOperator::Decrement => tacky::BinaryOperator::Subtract,
            };

            let (mut instructions, value) = emit_tacky_expression(*expression);
            let dst_name = unique_id::temp_variable_name();
            let dst = Value::Var(Var(dst_name.clone()));

            match value {
                Value::Var(var) => {
                    instructions.append(&mut vec![
                        Instruction::Copy(
                            Value::Var(var.clone()),
                            Value::Var(Var(dst_name.clone())),
                        ),
                        Instruction::Binary {
                            op: binary_op,
                            lhs: Value::Var(var.clone()),
                            rhs: Value::Constant(1),
                            dst: var.clone(),
                        },
                    ]);

                    (instructions, dst)
                }
                _ => unreachable!("Ran into an assignment with invalid lhs lvalue"),
            }
        }
        ast::Expression::Conditional {
            condition,
            then,
            r#else,
        } => {
            let (mut instructions, value) = emit_tacky_expression(*condition);
            let dst_name = unique_id::temp_variable_name();
            let dst = Value::Var(Var(dst_name.clone()));

            let else_label = unique_id::temp_label_name("conditional_else");
            let end_label = unique_id::temp_label_name("conditional_end");

            instructions.append(&mut vec![Instruction::JumpIfZero(
                value,
                else_label.clone(),
            )]);

            // then
            let (mut then_instructions, then_value) = emit_tacky_expression(*then);
            instructions.append(&mut then_instructions);
            instructions.append(&mut vec![
                Instruction::Copy(then_value, Value::Var(Var(dst_name.clone()))),
                Instruction::Jump(end_label.clone()),
                Instruction::Label(else_label),
            ]);

            // else
            let (mut else_instructions, else_value) = emit_tacky_expression(*r#else);
            instructions.append(&mut else_instructions);
            instructions.append(&mut vec![
                Instruction::Copy(else_value, Value::Var(Var(dst_name.clone()))),
                Instruction::Label(end_label),
            ]);

            (instructions, dst)
        }
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
        ast::Statement::If {
            condition,
            then,
            r#else,
        } => {
            let (mut instructions, value) = emit_tacky_expression(condition);
            let if_end_label = unique_id::temp_label_name("if_end");
            instructions.append(&mut vec![Instruction::JumpIfZero(
                value,
                if_end_label.clone(),
            )]);
            instructions.append(&mut emit_tacky_statement(*then));

            if let Some(stmt) = r#else {
                let else_end_label = unique_id::temp_label_name("else_end");
                instructions.append(&mut vec![
                    Instruction::Jump(else_end_label.clone()),
                    Instruction::Label(if_end_label),
                ]);
                instructions.append(&mut emit_tacky_statement(*stmt));
                instructions.append(&mut vec![Instruction::Label(else_end_label)]);
            } else {
                instructions.append(&mut vec![Instruction::Label(if_end_label)]);
            }

            instructions
        }
        ast::Statement::Goto(name) => vec![Instruction::Jump(name)],
        ast::Statement::Label(name, stmt) => {
            let mut instructions = vec![Instruction::Label(name)];
            instructions.append(&mut emit_tacky_statement(*stmt));
            instructions
        }
        ast::Statement::Compound(block) => emit_tacky_block(block),
        ast::Statement::Break(label) => vec![Instruction::Jump(format!("{label}_break"))],
        ast::Statement::Continue(label) => vec![Instruction::Jump(format!("{label}_continue"))],
        ast::Statement::While {
            condition,
            body,
            label,
        } => {
            let label = label.expect("If every path was successfull, this should be some.");
            let start = format!("{label}_continue");
            let end = format!("{label}_break");
            let mut instructions = vec![Instruction::Label(start.clone())];
            let (mut condition_instructions, condition_value) = emit_tacky_expression(condition);
            instructions.append(&mut condition_instructions);
            instructions.push(Instruction::JumpIfZero(condition_value, end.clone()));
            instructions.append(&mut emit_tacky_statement(*body));
            instructions.push(Instruction::Jump(start));
            instructions.push(Instruction::Label(end));
            instructions
        }
        ast::Statement::DoWhile {
            body,
            condition,
            label,
        } => {
            let label = label.expect("If every path was successfull, this should be some.");
            let start = unique_id::temp_label_name("do_while_start");
            let mut instructions = vec![Instruction::Label(start.clone())];
            instructions.append(&mut emit_tacky_statement(*body));
            instructions.push(Instruction::Label(format!("{label}_continue")));
            let (mut condition_instructions, condition_value) = emit_tacky_expression(condition);
            instructions.append(&mut condition_instructions);
            instructions.push(Instruction::JumpIfNotZero(condition_value, start));
            instructions.push(Instruction::Label(format!("{label}_break")));
            instructions
        }
        ast::Statement::For {
            init,
            condition,
            post,
            body,
            label,
        } => {
            let label = label.expect("If every path was successfull, this should be some.");
            let start = unique_id::temp_label_name("for_start");
            let continue_label = format!("{label}_continue");
            let break_label = format!("{label}_break");
            let mut instructions = emit_tacky_for_init(init);
            instructions.push(Instruction::Label(start.clone()));
            if let Some(condition) = condition {
                let (mut condition_instructions, condition_value) =
                    emit_tacky_expression(condition);
                instructions.append(&mut condition_instructions);
                instructions.push(Instruction::JumpIfZero(
                    condition_value,
                    break_label.clone(),
                ));
            }
            instructions.append(&mut emit_tacky_statement(*body));
            instructions.push(Instruction::Label(continue_label));
            if let Some(post) = post {
                instructions.append(&mut emit_tacky_expression(post).0);
            }
            instructions.push(Instruction::Jump(start));
            instructions.push(Instruction::Label(break_label));
            instructions
        }
        ast::Statement::Default(statement, label) => {
            let mut instructions = vec![Instruction::Label(label)];
            instructions.append(&mut emit_tacky_statement(*statement));
            instructions
        }
        ast::Statement::Case(_, statement, label) => {
            let mut instructions = vec![Instruction::Label(label)];
            instructions.append(&mut emit_tacky_statement(*statement));
            instructions
        }
        ast::Statement::Switch {
            expression,
            body,
            label: switch_label,
            cases,
        } => {
            let switch_label = format!(
                "{}_break",
                switch_label.expect("Switch label should be populated after all values.")
            );
            let mut default = None;
            let (mut instructions, value) = emit_tacky_expression(expression);
            let cases = cases.expect("Cases has to be Some after all passes.");
            for (key, label) in &cases {
                if let Some(constant) = key {
                    let dst_name = unique_id::temp_variable_name();
                    let dst = Var(dst_name.clone());

                    instructions.append(&mut vec![
                        Instruction::Binary {
                            op: tacky::BinaryOperator::Equal,
                            lhs: value.clone(),
                            rhs: Value::Constant(*constant),
                            dst: dst.clone(),
                        },
                        // 0 is false, everything else is true.
                        Instruction::JumpIfNotZero(Value::Var(dst), label.clone()),
                    ]);
                } else {
                    default = Some(label.clone());
                }
            }

            // Generate default / end jump
            instructions.push(Instruction::Jump(match default {
                Some(label) => label,
                None => switch_label.clone(),
            }));

            instructions.append(&mut emit_tacky_statement(*body));

            instructions.push(Instruction::Label(switch_label));

            instructions
        }
    }
}

pub fn emit_tacky_for_init(for_init: ast::ForInit) -> Vec<Instruction> {
    match for_init {
        ast::ForInit::InitDecl(decl) => emit_tacky_declaration(decl),
        ast::ForInit::InitExp(expr) => emit_tacky_expression(expr).0,
        ast::ForInit::None => vec![],
    }
}

pub fn emit_tacky_block(block: ast::Block) -> Vec<Instruction> {
    let mut instructions = vec![];
    for item in block.0 {
        instructions.append(&mut emit_tacky_block_item(item));
    }
    instructions
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

    body.append(&mut emit_tacky_block(func.body));

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
