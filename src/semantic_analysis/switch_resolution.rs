use std::collections::HashMap;

use thiserror::Error;

use crate::{
    ast::{Block, BlockItem, Expression, FunctionDeclaration, Program, Statement},
    unique_id,
};

#[derive(Debug, Error)]
pub enum SwitchResolutionError {
    #[error("Default label is not in a switch statement.")]
    DefaultNotInSwitch,
    #[error("Case label is not in a switch statement.")]
    CaseNotInSwitch,
    #[error("Case expression is not compiletime constant {0:?}")]
    ExpressionIsNotConstant(Expression),
    #[error("The switch case {0:?} is duplicated")]
    DuplicatedSwitchCase(Option<i32>),
}

pub fn resolve_program(program: Program) -> Result<Program, SwitchResolutionError> {
    let function_definition = resolve_function_definition(program.function_definition)?;
    Ok(Program {
        function_definition,
    })
}

fn resolve_function_definition(
    function: FunctionDeclaration,
) -> Result<FunctionDeclaration, SwitchResolutionError> {
    let mut data = None;
    let body = resolve_block(function.body, &mut data)?;
    Ok(FunctionDeclaration {
        name: function.name,
        body,
    })
}

fn resolve_block(
    block: Block,
    cases: &mut Option<HashMap<Option<i32>, String>>,
) -> Result<Block, SwitchResolutionError> {
    let mut items = vec![];

    for item in block.0 {
        items.push(resolve_block_item(item, cases)?);
    }

    Ok(Block(items))
}

fn resolve_block_item(
    item: BlockItem,
    data: &mut Option<HashMap<Option<i32>, String>>,
) -> Result<BlockItem, SwitchResolutionError> {
    match item {
        BlockItem::S(stmt) => Ok(BlockItem::S(resolve_statement(stmt, data)?)),
        BlockItem::D(decl) => Ok(BlockItem::D(decl)),
    }
}

fn resolve_constant(expression: Expression) -> Result<i32, SwitchResolutionError> {
    match expression {
        Expression::Constant(c) => Ok(c),
        expr => Err(SwitchResolutionError::ExpressionIsNotConstant(expr)),
    }
}

fn resolve_statement(
    stmt: Statement,
    cases: &mut Option<HashMap<Option<i32>, String>>,
) -> Result<Statement, SwitchResolutionError> {
    Ok(match stmt {
        Statement::If {
            condition,
            then,
            r#else,
        } => Statement::If {
            condition,
            then: Box::new(resolve_statement(*then, cases)?),
            r#else: match r#else {
                Some(stmt) => Some(Box::new(resolve_statement(*stmt, cases)?)),
                None => None,
            },
        },
        Statement::Label(label, stmt) => {
            Statement::Label(label, Box::new(resolve_statement(*stmt, cases)?))
        }
        Statement::Default(stmt, _) => match cases {
            None => return Err(SwitchResolutionError::DefaultNotInSwitch),
            Some(case_map) => {
                if case_map.contains_key(&None) {
                    return Err(SwitchResolutionError::DuplicatedSwitchCase(None));
                }
                let id = unique_id::temp_label("default");
                case_map.insert(None, id.clone());
                Statement::Default(Box::new(resolve_statement(*stmt, cases)?), id)
            }
        },
        Statement::Case(expr, stmt, _) => match cases {
            None => return Err(SwitchResolutionError::CaseNotInSwitch),
            Some(case_map) => {
                let key = resolve_constant(expr)?;
                let opt_key = Some(key);
                if case_map.contains_key(&opt_key) {
                    return Err(SwitchResolutionError::DuplicatedSwitchCase(opt_key));
                }
                let id = unique_id::temp_label("case");
                case_map.insert(opt_key, id.clone());
                Statement::Case(
                    Expression::Constant(key),
                    Box::new(resolve_statement(*stmt, cases)?),
                    id,
                )
            }
        },
        Statement::Compound(block) => Statement::Compound(resolve_block(block, cases)?),
        Statement::While {
            condition,
            body,
            label,
        } => Statement::While {
            condition,
            body: Box::new(resolve_statement(*body, cases)?),
            label,
        },
        Statement::DoWhile {
            body,
            condition,
            label,
        } => Statement::DoWhile {
            condition,
            body: Box::new(resolve_statement(*body, cases)?),
            label,
        },
        Statement::For {
            init,
            condition,
            post,
            body,
            label,
        } => Statement::For {
            init,
            condition,
            post,
            body: Box::new(resolve_statement(*body, cases)?),
            label,
        },
        Statement::Switch {
            expression,
            body,
            label,
            ..
        } => {
            let mut new_map = Some(HashMap::new());
            let new_body = Box::new(resolve_statement(*body, &mut new_map)?);
            Statement::Switch {
                expression,
                body: new_body,
                label,
                cases: new_map,
            }
        }

        stmt @ (Statement::Return(_)
        | Statement::Null
        | Statement::Expression(_)
        | Statement::Goto(_)
        | Statement::Continue(_)
        | Statement::Break(_)) => stmt,
    })
}
