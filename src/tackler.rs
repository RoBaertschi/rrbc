use crate::{
    ast,
    tacky::{FunctionDefiniton, Instruction, Program, Value, Var},
    unique_id,
};

pub fn emit_tacky_expression(expr: ast::Expression) -> (Vec<Instruction>, Value) {
    match expr {
        ast::Expression::Constant(val) => (vec![], Value::Constant(val)),
        ast::Expression::Unary(op, expr) => {
            let (mut inner_ins, val) = emit_tacky_expression(*expr);
            let dst_name = unique_id::temp();
            let dst = Value::Var(Var(dst_name.clone()));
            inner_ins.append(&mut vec![Instruction::Unary {
                operator: op.to_tacky(),
                src: val,
                dst: Var(dst_name.clone()),
            }]);

            (inner_ins, dst)
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
    }
}

pub fn emit_tacky_function(func: ast::FunctionDefinition) -> FunctionDefiniton {
    let body = emit_tacky_statement(func.body);
    FunctionDefiniton {
        identifier: func.name,
        body,
    }
}

pub fn emit_tacky_program(program: ast::Program) -> Program {
    Program(emit_tacky_function(program.function_definition))
}
