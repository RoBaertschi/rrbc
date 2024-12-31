use thiserror::Error;

use crate::{
    ast::{Block, BlockItem, FunctionDeclaration, Program, Statement},
    unique_id,
};

#[derive(Debug, Error)]
pub enum LoopLabelingError {
    #[error("A break statement is placed outside of a loop or switch statement.")]
    BreakOutsideLoopOrSwitch,
    #[error("A continue statement is placed outside of a loop.")]
    ContinueOutsideLoop,
}

pub fn label_program(program: Program) -> Result<Program, LoopLabelingError> {
    let mut new_funcs = vec![];

    for func in program.function_declarations {
        new_funcs.push(label_function_definition(func)?);
    }

    Ok(Program {
        function_declarations: new_funcs,
    })
}

fn label_function_definition(
    function: FunctionDeclaration,
) -> Result<FunctionDeclaration, LoopLabelingError> {
    Ok(FunctionDeclaration {
        name: function.name,
        body: function.body.map_or(Ok(None), |some| {
            label_block(some, None, None).map(Some)
        })?,
        params: function.params,
    })
}

fn label_block(
    block: Block,
    current_label: Option<String>,
    current_switch: Option<String>,
) -> Result<Block, LoopLabelingError> {
    let mut items = vec![];

    for item in block.0 {
        items.push(label_block_item(
            item,
            current_label.clone(),
            current_switch.clone(),
        )?);
    }

    Ok(Block(items))
}

fn label_block_item(
    item: BlockItem,
    current_label: Option<String>,
    current_switch: Option<String>,
) -> Result<BlockItem, LoopLabelingError> {
    match item {
        BlockItem::S(stmt) => Ok(BlockItem::S(label_statement(
            stmt,
            current_label,
            current_switch,
        )?)),
        BlockItem::D(decl) => Ok(BlockItem::D(decl)),
    }
}

fn label_statement(
    statement: Statement,
    current_label: Option<String>,
    current_switch: Option<String>,
) -> Result<Statement, LoopLabelingError> {
    match statement {
        Statement::If {
            condition,
            then,
            r#else,
        } => Ok(Statement::If {
            condition,
            then: Box::new(label_statement(
                *then,
                current_label.clone(),
                current_switch.clone(),
            )?),
            r#else: match r#else {
                None => None,
                Some(statement) => Some(Box::new(label_statement(
                    *statement,
                    current_label,
                    current_switch,
                )?)),
            },
        }),
        Statement::Label(ident, body) => {
            let body = Box::new(label_statement(*body, current_label, current_switch)?);
            Ok(Statement::Label(ident, body))
        }
        Statement::Compound(block) => Ok(Statement::Compound(label_block(
            block,
            current_label,
            current_switch,
        )?)),
        Statement::Break(_) => match (current_label, current_switch) {
            (_, Some(switch)) => Ok(Statement::Break(switch)),
            (Some(label), None) => Ok(Statement::Break(label)),
            (None, None) => Err(LoopLabelingError::BreakOutsideLoopOrSwitch),
        },
        Statement::Continue(_) => {
            if let Some(label) = current_label {
                Ok(Statement::Continue(label))
            } else {
                Err(LoopLabelingError::ContinueOutsideLoop)
            }
        }
        Statement::While {
            condition, body, ..
        } => {
            let label = unique_id::temp_loop_label("while");
            let body = Box::new(label_statement(*body, Some(label.clone()), None)?);
            Ok(Statement::While {
                condition,
                body,
                label: Some(label),
            })
        }
        Statement::DoWhile {
            body, condition, ..
        } => {
            let label = unique_id::temp_loop_label("dowhile");
            let body = Box::new(label_statement(*body, Some(label.clone()), None)?);
            Ok(Statement::DoWhile {
                condition,
                body,
                label: Some(label),
            })
        }
        Statement::For {
            init,
            condition,
            post,
            body,
            ..
        } => {
            let label = unique_id::temp_loop_label("for");
            let body = Box::new(label_statement(*body, Some(label.clone()), None)?);
            Ok(Statement::For {
                init,
                condition,
                post,
                body,
                label: Some(label),
            })
        }
        Statement::Default(statement, label) => Ok(Statement::Default(
            Box::new(label_statement(*statement, current_label, current_switch)?),
            label,
        )),
        Statement::Case(expression, statement, label) => Ok(Statement::Case(
            expression,
            Box::new(label_statement(*statement, current_label, current_switch)?),
            label,
        )),
        Statement::Switch {
            expression,
            body,
            cases: data,
            label: _,
        } => {
            let id = unique_id::temp_switch_label();
            Ok(Statement::Switch {
                label: Some(id.clone()),
                expression,
                body: Box::new(label_statement(*body, current_label, Some(id))?),
                cases: data,
            })
        }
        stmt @ (Statement::Return(_)
        | Statement::Null
        | Statement::Goto(_)
        | Statement::Expression(_)) => Ok(stmt),
    }
}
