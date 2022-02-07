use proc_macro2::TokenStream;
use syn::parse::{Parse, ParseStream};

use syn::{Expr, Token};

struct ModuloExpr {
    modulo: Expr,
    _delim: Token![;],
    expr: Expr,
}

impl Parse for ModuloExpr {
    fn parse(input: ParseStream) -> syn::parse::Result<ModuloExpr> {
        Ok(ModuloExpr {
            modulo: input.parse()?,
            _delim: input.parse()?,
            expr: input.parse()?,
        })
    }
}

pub fn modulo(tokens: TokenStream) -> TokenStream {
    use quote::ToTokens;
    let ModuloExpr {
        modulo,
        _delim,
        expr,
    } = syn::parse2::<ModuloExpr>(tokens).unwrap();
    convert(&modulo, expr).to_token_stream()
}

fn convert(modulo: &Expr, expr: Expr) -> Expr {
    match expr {
        Expr::Binary(expr) => {
            let left = convert(modulo, *expr.left);
            let right = convert(modulo, *expr.right);
            match expr.op {
                syn::BinOp::Add(_) => {
                    syn::parse_quote!(modulo_internals::add(#left, #right, #modulo))
                }
                syn::BinOp::Sub(_) => {
                    syn::parse_quote!(modulo_internals::sub(#left, #right, #modulo))
                }
                syn::BinOp::Mul(_) => {
                    syn::parse_quote!(modulo_internals::mul(#left, #right, #modulo))
                }
                _ => Expr::Binary(syn::ExprBinary {
                    attrs: expr.attrs,
                    left: left.into(),
                    op: expr.op,
                    right: right.into(),
                }),
            }
        }
        _ => expr,
    }
}

pub fn add(a: u32, b: u32, modulo: u32) -> u32 {
    if a < modulo - b {
        a + b
    } else {
        a - (modulo - b)
    }
}

pub fn sub(a: u32, b: u32, modulo: u32) -> u32 {
    if a >= b {
        a - b
    } else {
        a + (modulo - b)
    }
}

pub fn mul(a: u32, b: u32, modulo: u32) -> u32 {
    (a as u64 * b as u64 % modulo as u64) as u32
}

