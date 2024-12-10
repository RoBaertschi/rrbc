use thiserror::Error;

use crate::{
    ast::{Block, BlockItem, FunctionDefinition, Program, Statement},
    unique_id,
};

#[derive(Debug, Error)]
pub enum LoopLabelingError {
    #[error("A break statement is placed outside of a loop.")]
    BreakOutsideLoop,
    #[error("A continue statement is placed outside of a loop.")]
    ContinueOutsideLoop,
}

pub fn label_program(program: Program) -> Result<Program, LoopLabelingError> {
    let function = label_function_definition(program.function_definition)?;

    Ok(Program {
        function_definition: function,
    })
}

fn label_function_definition(
    function: FunctionDefinition,
) -> Result<FunctionDefinition, LoopLabelingError> {
    let body = label_block(function.body, None)?;
    Ok(FunctionDefinition {
        name: function.name,
        body,
    })
}

fn label_block(block: Block, current_label: Option<String>) -> Result<Block, LoopLabelingError> {
    let mut items = vec![];

    for item in block.0 {
        items.push(label_block_item(item, current_label.clone())?);
    }

    Ok(Block(items))
}

fn label_block_item(
    item: BlockItem,
    current_label: Option<String>,
) -> Result<BlockItem, LoopLabelingError> {
    match item {
        BlockItem::S(stmt) => Ok(BlockItem::S(label_statement(stmt, current_label)?)),
        BlockItem::D(decl) => Ok(BlockItem::D(decl)),
    }
}

fn label_statement(
    statement: Statement,
    current_label: Option<String>,
) -> Result<Statement, LoopLabelingError> {
    match statement {
        Statement::Return(expr) => Ok(Statement::Return(expr)),
        Statement::Expression(expr) => Ok(Statement::Expression(expr)),
        Statement::If {
            condition,
            then,
            r#else,
        } => Ok(Statement::If {
            condition,
            then: Box::new(label_statement(*then, current_label.clone())?),
            r#else: match r#else {
                None => None,
                Some(statement) => Some(Box::new(label_statement(*statement, current_label)?)),
            },
        }),
        Statement::Goto(ident) => Ok(Statement::Goto(ident)),
        Statement::Label(ident, body) => {
            let body = Box::new(label_statement(*body, current_label)?);
            Ok(Statement::Label(ident, body))
        }
        Statement::Compound(block) => Ok(Statement::Compound(label_block(block, current_label)?)),
        Statement::Break(_) => {
            if let Some(label) = current_label {
                Ok(Statement::Break(label))
            } else {
                Err(LoopLabelingError::BreakOutsideLoop)
            }
        }
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
            let body = Box::new(label_statement(*body, Some(label.clone()))?);
            Ok(Statement::While {
                condition,
                body,
                label,
            })
        }
        Statement::DoWhile {
            body, condition, ..
        } => {
            let label = unique_id::temp_loop_label("dowhile");
            let body = Box::new(label_statement(*body, Some(label.clone()))?);
            Ok(Statement::DoWhile {
                condition,
                body,
                label,
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
            let body = Box::new(label_statement(*body, Some(label.clone()))?);
            Ok(Statement::For {
                init,
                condition,
                post,
                body,
                label,
            })
        }
        Statement::Null => Ok(Statement::Null),
    }
}
