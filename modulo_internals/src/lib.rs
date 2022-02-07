use proc_macro2::TokenStream;
use syn::parse::{Parse, ParseStream};

use syn::{BinOp, Expr, Token, UnOp};

struct ModuloExpr {
    expr: Expr,
    _delim: Token![;],
    modulo: Expr,
}

impl Parse for ModuloExpr {
    fn parse(input: ParseStream) -> syn::parse::Result<ModuloExpr> {
        Ok(ModuloExpr {
            expr: input.parse()?,
            _delim: input.parse()?,
            modulo: input.parse()?,
        })
    }
}

pub fn modulo(tokens: TokenStream) -> TokenStream {
    use quote::ToTokens;
    let ModuloExpr {
        mut expr, modulo, ..
    } = syn::parse2(tokens).unwrap();
    expr.convert(&modulo);
    expr.to_token_stream()
}

trait Convert {
    fn convert(&mut self, modulo: &Expr);
}

impl Convert for Expr {
    fn convert(&mut self, modulo: &Expr) {
        match self {
            Expr::Lit(_) => {}
            Expr::Path(_) => {}
            Expr::Assign(expr) => {
                expr.left.convert(modulo);
                expr.right.convert(modulo);
            }
            Expr::Unary(syn::ExprUnary { expr, op, .. }) => {
                expr.convert(modulo);
                *self = match op {
                    UnOp::Neg(_) => syn::parse_quote! {
                        modulo_internals::sub(0, #expr, #modulo)
                    },
                    _ => todo!(),
                }
            }
            Expr::Binary(syn::ExprBinary {
                left, op, right, ..
            }) => {
                left.convert(modulo);
                right.convert(modulo);
                *self = match op {
                    BinOp::Add(_) => syn::parse_quote! {
                        modulo_internals::add(#left, #right, #modulo)
                    },
                    BinOp::Sub(_) => syn::parse_quote! {
                        modulo_internals::sub(#left, #right, #modulo)
                    },
                    BinOp::Mul(_) => syn::parse_quote! {
                        modulo_internals::mul(#left, #right, #modulo)
                    },
                    BinOp::Div(_) => syn::parse_quote! {
                        modulo_internals::div(#left, #right, #modulo)
                    },
                    _ => todo!(),
                }
            }
            Expr::AssignOp(syn::ExprAssignOp {
                left, op, right, ..
            }) => {
                left.convert(modulo);
                right.convert(modulo);
                *self = match op {
                    BinOp::AddEq(_) => syn::parse_quote! {
                        modulo_internals::add_assign(#right, &mut #left, #modulo)
                    },
                    BinOp::SubEq(_) => syn::parse_quote! {
                        modulo_internals::sub_assign(#right, &mut #left, #modulo)
                    },
                    BinOp::MulEq(_) => syn::parse_quote! {
                        modulo_internals::mul_assign(#right, &mut #left, #modulo)
                    },
                    BinOp::DivEq(_) => syn::parse_quote! {
                        modulo_internals::div_assign(#right, &mut #left, #modulo)
                    },
                    _ => todo!(),
                }
            }
            Expr::Array(expr) => {
                for elem in &mut expr.elems {
                    elem.convert(modulo);
                }
            }
            _ => todo!(),
        }
    }
}

pub fn add(a: u32, b: u32, modulo: u32) -> u32 {
    if a < modulo - b {
        a + b
    } else {
        a - (modulo - b)
    }
}

#[test]
fn test_add() {
    assert_eq!(add(5, 8, 10), 3);
    assert_eq!(add(u32::MAX - 1, u32::MAX - 1, u32::MAX), u32::MAX - 2);
}

pub fn sub(a: u32, b: u32, modulo: u32) -> u32 {
    if a >= b {
        a - b
    } else {
        a + (modulo - b)
    }
}

#[test]
fn test_sub() {
    assert_eq!(sub(5, 8, 10), 7);
    assert_eq!(sub(0, 1, u32::MAX), u32::MAX - 1);
    assert_eq!(sub(u32::MAX - 1, 0, u32::MAX), u32::MAX - 1);
    assert_eq!(sub(0, u32::MAX - 1, u32::MAX), 1);
}

pub fn mul(a: u32, b: u32, modulo: u32) -> u32 {
    (a as u64 * b as u64 % modulo as u64) as u32
}

pub fn div(a: u32, b: u32, modulo: u32) -> u32 {
    mul(a, inv(b, modulo), modulo)
}

pub fn pow(mut x: u32, mut n: u32, modulo: u32) -> u32 {
    let mut ret = 1;
    while n != 0 {
        if n % 2 != 0 {
            mul_assign(x, &mut ret, modulo);
        }
        mul_assign(x, &mut x, modulo);
        n /= 2;
    }
    ret
}

#[test]
fn test_pow() {
    assert_eq!(pow(2, 0, 10), 1);
    assert_eq!(pow(2, 3, 10), 8);
    assert_eq!(pow(2, 5, 10), 2);
}

pub fn inv(x: u32, modulo: u32) -> u32 {
    pow(x, modulo - 2, modulo)
}

#[test]
fn test_inv() {
    let modulo = 1_000_000_007;
    for i in 1..100 {
        assert_eq!(mul(i, inv(i, modulo), modulo), 1);
    }
}

pub fn add_assign(src: u32, dest: &mut u32, modulo: u32) {
    *dest = add(*dest, src, modulo);
}

pub fn sub_assign(src: u32, dest: &mut u32, modulo: u32) {
    *dest = sub(*dest, src, modulo);
}

pub fn mul_assign(src: u32, dest: &mut u32, modulo: u32) {
    *dest = mul(*dest, src, modulo);
}

pub fn div_assign(src: u32, dest: &mut u32, modulo: u32) {
    *dest = div(*dest, src, modulo);
}
