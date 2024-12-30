use std::collections::HashMap;

use thiserror::Error;

use crate::{
    ast::{
        Block, BlockItem, Declaration, Expression, ForInit, FunctionDeclaration, Program,
        Statement, VariableDeclaration,
    },
    types::Type,
    utils::ResultOkMap,
};

pub type Symbols = HashMap<String, (Type, bool)>;

#[derive(Error, Debug)]
pub enum TypeCheckingError {
    #[error(
        "Incompatible function declarations named \"{0}\", expected {1:?} parameters, got {2:?}"
    )]
    IncompatibleFunctionDeclarations(String, Type, Type),
    #[error("Function {0} was redefined")]
    FunctionRedefinition(String),
    #[error("Could not find function \"{0}\" declaration")]
    FunctionNotFound(String),
    #[error("Function \"{0}\" was called with an incorrect amount of arguments")]
    FunctionIncorrectArgsCount(String),
    #[error("Function \"{0}\" used as a variable.")]
    FunctionUsedAsVariable(String),
    #[error("Variable \"{0}\" used as a function.")]
    VariableUsedAsFunction(String),
    #[error("Could not find variable \"{0}\"")]
    VariableNotFound(String),
}

pub fn typecheck_program(program: Program) -> Result<(Program, Symbols), TypeCheckingError> {
    let mut symbols = Symbols::new();
    let mut new_funcs = vec![];
    for func in program.function_declarations {
        new_funcs.push(typecheck_function_declaration(func, &mut symbols)?);
    }

    Ok((
        Program {
            function_declarations: new_funcs,
        },
        symbols,
    ))
}

fn typecheck_variable_declaration(
    decl: VariableDeclaration,
    symbols: &mut Symbols,
) -> Result<VariableDeclaration, TypeCheckingError> {
    symbols.insert(decl.name.clone(), (Type::Int, true));

    let exp = match decl.exp {
        Some(expr) => Some(typecheck_expression(expr, symbols)?),
        None => None,
    };
    Ok(VariableDeclaration {
        name: decl.name,
        exp,
    })
}

fn typecheck_function_declaration(
    decl: FunctionDeclaration,
    symbols: &mut Symbols,
) -> Result<FunctionDeclaration, TypeCheckingError> {
    let fun_type = Type::FunType(decl.params.len());
    let has_body = decl.body.is_some();
    let mut already_defined = false;

    if let Some((old_decl @ Type::FunType(_), defined)) = symbols.get(&decl.name) {
        if *old_decl != fun_type {
            return Err(TypeCheckingError::IncompatibleFunctionDeclarations(
                decl.name, *old_decl, fun_type,
            ));
        }
        already_defined = *defined;

        if already_defined && has_body {
            return Err(TypeCheckingError::FunctionRedefinition(decl.name));
        }
    }

    symbols.insert(decl.name.clone(), (fun_type, already_defined || has_body));

    let body = decl.body.ok_map(|body| {
        for param in decl.params.clone() {
            symbols.insert(param, (Type::Int, true));
        }
        typecheck_block(body, symbols)
    })?;

    Ok(FunctionDeclaration {
        name: decl.name,
        params: decl.params,
        body,
    })
}

fn typecheck_declaration(
    decl: Declaration,
    symbols: &mut Symbols,
) -> Result<Declaration, TypeCheckingError> {
    Ok(match decl {
        Declaration::FunDecl(function_declaration) => Declaration::FunDecl(
            typecheck_function_declaration(function_declaration, symbols)?,
        ),
        Declaration::VarDecl(variable_declaration) => Declaration::VarDecl(
            typecheck_variable_declaration(variable_declaration, symbols)?,
        ),
    })
}

fn typecheck_for_init(
    for_init: ForInit,
    symbols: &mut Symbols,
) -> Result<ForInit, TypeCheckingError> {
    Ok(match for_init {
        ForInit::InitDecl(variable_declaration) => ForInit::InitDecl(
            typecheck_variable_declaration(variable_declaration, symbols)?,
        ),
        ForInit::InitExp(expression) => {
            ForInit::InitExp(typecheck_expression(expression, symbols)?)
        }
        ForInit::None => ForInit::None,
    })
}

fn typecheck_block(block: Block, symbols: &mut Symbols) -> Result<Block, TypeCheckingError> {
    let mut new_items = vec![];
    for item in block.0 {
        new_items.push(typecheck_block_item(item, symbols)?);
    }
    Ok(Block(new_items))
}

fn typecheck_block_item(
    block_item: BlockItem,
    symbols: &mut Symbols,
) -> Result<BlockItem, TypeCheckingError> {
    Ok(match block_item {
        BlockItem::S(statement) => BlockItem::S(typecheck_statement(statement, symbols)?),
        BlockItem::D(declaration) => BlockItem::D(typecheck_declaration(declaration, symbols)?),
    })
}

fn typecheck_statement(
    stmt: Statement,
    symbols: &mut Symbols,
) -> Result<Statement, TypeCheckingError> {
    Ok(match stmt {
        Statement::Return(expression) => {
            Statement::Return(typecheck_expression(expression, symbols)?)
        }
        Statement::Expression(expression) => {
            Statement::Expression(typecheck_expression(expression, symbols)?)
        }
        Statement::If {
            condition,
            then,
            r#else,
        } => Statement::If {
            condition: typecheck_expression(condition, symbols)?,
            then: Box::new(typecheck_statement(*then, symbols)?),
            r#else: r#else
                .map(|r#else| typecheck_statement(*r#else, symbols).map(Box::new))
                .transpose()?,
        },
        Statement::Label(ident, statement) => {
            Statement::Label(ident, Box::new(typecheck_statement(*statement, symbols)?))
        }
        Statement::Default(statement, label) => {
            Statement::Default(Box::new(typecheck_statement(*statement, symbols)?), label)
        }
        Statement::Case(expression, statement, label) => Statement::Case(
            typecheck_expression(expression, symbols)?,
            Box::new(typecheck_statement(*statement, symbols)?),
            label,
        ),
        Statement::Compound(block) => Statement::Compound(typecheck_block(block, symbols)?),
        Statement::While {
            condition,
            body,
            label,
        } => Statement::While {
            condition: typecheck_expression(condition, symbols)?,
            body: Box::new(typecheck_statement(*body, symbols)?),
            label,
        },
        Statement::DoWhile {
            body,
            condition,
            label,
        } => Statement::While {
            body: Box::new(typecheck_statement(*body, symbols)?),
            condition: typecheck_expression(condition, symbols)?,
            label,
        },
        Statement::For {
            init,
            condition,
            post,
            body,
            label,
        } => Statement::For {
            init: typecheck_for_init(init, symbols)?,
            condition: condition.ok_map(|condition| typecheck_expression(condition, symbols))?,
            post: post.ok_map(|post| typecheck_expression(post, symbols))?,
            body: Box::new(typecheck_statement(*body, symbols)?),
            label,
        },
        Statement::Switch {
            expression,
            body,
            label,
            cases,
        } => Statement::Switch {
            expression: typecheck_expression(expression, symbols)?,
            body: Box::new(typecheck_statement(*body, symbols)?),
            label,
            cases,
        },
        stmt @ (Statement::Null
        | Statement::Goto(_)
        | Statement::Continue(_)
        | Statement::Break(_)) => stmt,
    })
}

fn typecheck_expression(
    expr: Expression,
    symbols: &mut Symbols,
) -> Result<Expression, TypeCheckingError> {
    Ok(match expr {
        Expression::Var(v) => {
            match symbols.get(&v) {
                Some((t, _)) => {
                    if !matches!(t, Type::Int) {
                        return Err(TypeCheckingError::FunctionUsedAsVariable(v));
                    }
                }
                None => return Err(TypeCheckingError::VariableNotFound(v)),
            }

            Expression::Var(v)
        }
        Expression::FunctionCall(f, args) => {
            let fun_type = match symbols.get(&f) {
                Some((t, _)) => t,
                None => return Err(TypeCheckingError::FunctionNotFound(f)),
            };

            match fun_type {
                Type::Int => return Err(TypeCheckingError::VariableUsedAsFunction(f)),
                Type::FunType(args_count) if *args_count != args.len() => {
                    return Err(TypeCheckingError::FunctionIncorrectArgsCount(f))
                }
                _ => {}
            };

            let mut new_args = vec![];
            for arg in args {
                new_args.push(typecheck_expression(arg, symbols)?);
            }

            Expression::FunctionCall(f, new_args)
        }
        Expression::Unary { op, expression } => Expression::Unary {
            op,
            expression: Box::new(typecheck_expression(*expression, symbols)?),
        },
        Expression::Binary { op, lhs, rhs } => Expression::Binary {
            op,
            lhs: Box::new(typecheck_expression(*lhs, symbols)?),
            rhs: Box::new(typecheck_expression(*rhs, symbols)?),
        },
        Expression::Assignment { op, lhs, rhs } => Expression::Assignment {
            op,
            lhs: Box::new(typecheck_expression(*lhs, symbols)?),
            rhs: Box::new(typecheck_expression(*rhs, symbols)?),
        },
        Expression::Conditional {
            condition,
            then,
            r#else,
        } => Expression::Conditional {
            condition: Box::new(typecheck_expression(*condition, symbols)?),
            then: Box::new(typecheck_expression(*then, symbols)?),
            r#else: Box::new(typecheck_expression(*r#else, symbols)?),
        },
        Expression::Postfix(postfix_operator, expression) => Expression::Postfix(
            postfix_operator,
            Box::new(typecheck_expression(*expression, symbols)?),
        ),
        expr @ Expression::Constant(_) => expr,
    })
}
