//! TODO: Move this into the Parser for support with typedefs someday.

use std::collections::HashMap;

use thiserror::Error;

use crate::{
    ast::{
        Block, BlockItem, Declaration, Expression, ForInit, FunctionDeclaration, Identifier,
        Program, Statement, UnaryOperator, VariableDeclaration,
    },
    unique_id,
};

#[derive(Error, Debug)]
pub enum IdentifierResolutionError {
    #[error("Function {0} is not allowed to have a definition in a function definition")]
    NestedFunctionDefinition(String),
    #[error("Function {0} was redeclared in the same scope")]
    FunctionNameRedeclaration(String),
    #[error("Function {0} was not found in the current scope")]
    FunctionNameNotFound(String),
    #[error("Variable {0} was not declared in this scope")]
    VariableNameNotFound(String),
    #[error("Variable {0} was redeclared in the same scope")]
    VariableNameRedeclaration(String),
    #[error("Invalid assignment on non lvalue {0:?}")]
    InvalidLvalue(Expression),
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
struct IdentifierMapEntry {
    new_name: String,
    from_current_scope: bool,
    external_linkage: bool,
}
type IdentifierMap = HashMap<String, IdentifierMapEntry>;

fn copy_identifier_map(identifier_map: &IdentifierMap) -> IdentifierMap {
    // Expensive but necessary
    let mut new_identifier_map = identifier_map.clone();
    for map_entry in new_identifier_map.iter_mut() {
        map_entry.1.from_current_scope = false;
    }
    new_identifier_map
}

pub fn resolve_program(program: Program) -> Result<Program, IdentifierResolutionError> {
    let mut identifier_map = IdentifierMap::new();

    let mut new_funcs = vec![];

    for func in program.function_declarations {
        new_funcs.push(resolve_function_declaration(&mut identifier_map, func)?);
    }

    Ok(Program {
        function_declarations: new_funcs,
    })
}

fn resolve_function_declaration(
    identifier_map: &mut IdentifierMap,
    function_declaration: FunctionDeclaration,
) -> Result<FunctionDeclaration, IdentifierResolutionError> {
    match identifier_map.get(&function_declaration.name) {
        Some(entry) if entry.from_current_scope && !entry.external_linkage => {
            return Err(IdentifierResolutionError::FunctionNameRedeclaration(
                function_declaration.name.clone(),
            ))
        }
        _ => {}
    }

    identifier_map.insert(
        function_declaration.name.clone(),
        IdentifierMapEntry {
            new_name: function_declaration.name.clone(),
            from_current_scope: true,
            external_linkage: true,
        },
    );

    let mut new_scope = copy_identifier_map(identifier_map);
    let mut new_params = vec![];

    for param in function_declaration.params {
        new_params.push(resolve_variable_like(&mut new_scope, param)?);
    }

    Ok(FunctionDeclaration {
        name: function_declaration.name,
        params: new_params,
        body: match function_declaration.body {
            Some(block) => Some(resolve_block(&mut new_scope, block)?),
            None => None,
        },
    })
}

fn resolve_variable_like(
    identifier_map: &mut IdentifierMap,
    ident: Identifier,
) -> Result<Identifier, IdentifierResolutionError> {
    if let Some(entry) = identifier_map.get(&ident) {
        if entry.from_current_scope {
            return Err(IdentifierResolutionError::VariableNameRedeclaration(ident));
        }
    }

    let unique_name = unique_id::temp_c_variable_name(&ident);
    identifier_map.insert(
        ident,
        IdentifierMapEntry {
            new_name: unique_name.clone(),
            from_current_scope: true,
            external_linkage: false,
        },
    );

    Ok(unique_name)
}

fn resolve_block(
    identifier_map: &mut IdentifierMap,
    block: Block,
) -> Result<Block, IdentifierResolutionError> {
    let mut blocks = vec![];
    for item in block.0 {
        blocks.push(resolve_block_item(identifier_map, item)?);
    }

    Ok(Block(blocks))
}

fn resolve_block_item(
    identifier_map: &mut IdentifierMap,
    block_item: BlockItem,
) -> Result<BlockItem, IdentifierResolutionError> {
    Ok(match block_item {
        BlockItem::S(s) => BlockItem::S(resolve_statement(identifier_map, s)?),
        BlockItem::D(d) => BlockItem::D(resolve_declaration(identifier_map, d)?),
    })
}

fn resolve_declaration(
    identifier_map: &mut IdentifierMap,
    decl: Declaration,
) -> Result<Declaration, IdentifierResolutionError> {
    Ok(match decl {
        Declaration::FunDecl(FunctionDeclaration {
            name,
            body: Some(_),
            ..
        }) => return Err(IdentifierResolutionError::NestedFunctionDefinition(name)),
        Declaration::FunDecl(function_declaration) => Declaration::FunDecl(
            resolve_function_declaration(identifier_map, function_declaration)?,
        ),
        Declaration::VarDecl(variable_declaration) => Declaration::VarDecl(
            resolve_variable_declaration(identifier_map, variable_declaration)?,
        ),
    })
}

fn resolve_variable_declaration(
    identifier_map: &mut IdentifierMap,
    decl: VariableDeclaration,
) -> Result<VariableDeclaration, IdentifierResolutionError> {
    let unique_name = resolve_variable_like(identifier_map, decl.name)?;

    let expr = if let Some(expr) = decl.exp {
        Some(resolve_expression(identifier_map, expr)?)
    } else {
        None
    };

    Ok(VariableDeclaration {
        name: unique_name,
        exp: expr,
    })
}

fn resolve_statement(
    identifier_map: &mut IdentifierMap,
    stmt: Statement,
) -> Result<Statement, IdentifierResolutionError> {
    Ok(match stmt {
        Statement::Return(expr) => Statement::Return(resolve_expression(identifier_map, expr)?),
        Statement::Expression(expr) => {
            Statement::Expression(resolve_expression(identifier_map, expr)?)
        }
        Statement::Null => Statement::Null,
        Statement::If {
            condition,
            then,
            r#else,
        } => {
            let resolved_else = match r#else {
                Some(r#else) => Some(Box::new(resolve_statement(identifier_map, *r#else)?)),
                None => None,
            };
            Statement::If {
                condition: resolve_expression(identifier_map, condition)?,
                then: Box::new(resolve_statement(identifier_map, *then)?),
                r#else: resolved_else,
            }
        }
        Statement::Goto(ident) => Statement::Goto(ident),
        Statement::Label(ident, stmt) => {
            Statement::Label(ident, Box::new(resolve_statement(identifier_map, *stmt)?))
        }
        Statement::Compound(block) => {
            let mut new_identifier_map = copy_identifier_map(identifier_map);
            Statement::Compound(resolve_block(&mut new_identifier_map, block)?)
        }
        Statement::Break(label) => Statement::Break(label),
        Statement::Continue(label) => Statement::Continue(label),
        Statement::While {
            condition,
            body,
            label,
        } => Statement::While {
            condition: resolve_expression(identifier_map, condition)?,
            body: Box::new(resolve_statement(identifier_map, *body)?),
            label,
        },
        Statement::DoWhile {
            body,
            condition,
            label,
        } => Statement::DoWhile {
            condition: resolve_expression(identifier_map, condition)?,
            body: Box::new(resolve_statement(identifier_map, *body)?),
            label,
        },
        Statement::For {
            init,
            condition,
            post,
            body,
            label,
        } => {
            let mut new_identifier_map = copy_identifier_map(identifier_map);
            Statement::For {
                init: resolve_for_init(&mut new_identifier_map, init)?,
                condition: resolve_optional_expression(&mut new_identifier_map, condition)?,
                post: resolve_optional_expression(&mut new_identifier_map, post)?,
                body: Box::new(resolve_statement(&mut new_identifier_map, *body)?),
                label,
            }
        }
        Statement::Default(stmt, label) => {
            Statement::Default(Box::new(resolve_statement(identifier_map, *stmt)?), label)
        }
        Statement::Case(expr, stmt, label) => Statement::Case(
            expr,
            Box::new(resolve_statement(identifier_map, *stmt)?),
            label,
        ),
        Statement::Switch {
            expression,
            body,
            label,
            cases,
        } => Statement::Switch {
            expression: resolve_expression(identifier_map, expression)?,
            body: Box::new(resolve_statement(identifier_map, *body)?),
            label,
            cases,
        },
    })
}

fn resolve_optional_expression(
    identifier_map: &mut IdentifierMap,
    expr: Option<Expression>,
) -> Result<Option<Expression>, IdentifierResolutionError> {
    match expr {
        Some(expr) => Ok(Some(resolve_expression(identifier_map, expr)?)),
        None => Ok(None),
    }
}

fn resolve_for_init(
    identifier_map: &mut IdentifierMap,
    init: ForInit,
) -> Result<ForInit, IdentifierResolutionError> {
    match init {
        ForInit::InitDecl(declaration) => Ok(ForInit::InitDecl(resolve_variable_declaration(
            identifier_map,
            declaration,
        )?)),
        ForInit::InitExp(expression) => Ok(ForInit::InitExp(resolve_expression(
            identifier_map,
            expression,
        )?)),
        ForInit::None => Ok(ForInit::None),
    }
}

fn resolve_variable(
    identifier_map: &mut IdentifierMap,
    expr: Expression,
) -> Result<Expression, IdentifierResolutionError> {
    match expr {
        Expression::Var(name) => Ok(Expression::Var(
            identifier_map
                .get(&name)
                .ok_or(IdentifierResolutionError::VariableNameNotFound(name))?
                .new_name
                .clone(),
        )),

        expr => Err(IdentifierResolutionError::InvalidLvalue(expr)),
    }
}

fn resolve_expression(
    identifier_map: &mut IdentifierMap,
    expr: Expression,
) -> Result<Expression, IdentifierResolutionError> {
    Ok(match expr {
        Expression::Var(name) => match identifier_map.get(&name) {
            Some(entry) => Expression::Var(entry.new_name.clone()),
            None => return Err(IdentifierResolutionError::VariableNameNotFound(name)),
        },
        Expression::Assignment { lhs, rhs, op } => Expression::Assignment {
            op,
            lhs: Box::new(resolve_variable(identifier_map, *lhs)?),
            rhs: Box::new(resolve_expression(identifier_map, *rhs)?),
        },
        Expression::Unary {
            op: op @ (UnaryOperator::Decrement | UnaryOperator::Increment),
            expression: expr,
        } => Expression::Unary {
            op,
            expression: Box::new(resolve_variable(identifier_map, *expr)?),
        },
        Expression::Postfix(postfix_operator, expression) => Expression::Postfix(
            postfix_operator,
            Box::new(resolve_variable(identifier_map, *expression)?),
        ),
        Expression::Unary {
            op,
            expression: expr,
        } => Expression::Unary {
            op,
            expression: Box::new(resolve_expression(identifier_map, *expr)?),
        },
        Expression::Binary { op, lhs, rhs } => Expression::Binary {
            op,
            lhs: Box::new(resolve_expression(identifier_map, *lhs)?),
            rhs: Box::new(resolve_expression(identifier_map, *rhs)?),
        },
        Expression::Constant(constant) => Expression::Constant(constant),
        Expression::Conditional {
            condition,
            then,
            r#else,
        } => Expression::Conditional {
            condition: Box::new(resolve_expression(identifier_map, *condition)?),
            then: Box::new(resolve_expression(identifier_map, *then)?),
            r#else: Box::new(resolve_expression(identifier_map, *r#else)?),
        },
        Expression::FunctionCall(ident, args) => match identifier_map.get(&ident) {
            Some(entry) => {
                let new_fun_name = entry.new_name.clone();
                let mut new_args = vec![];
                for arg in args {
                    new_args.push(resolve_expression(identifier_map, arg)?);
                }
                Expression::FunctionCall(new_fun_name, new_args)
            }
            None => return Err(IdentifierResolutionError::FunctionNameNotFound(ident)),
        },
    })
}
