use proc_macro2::TokenStream;
use syn::parse::{Parse, ParseStream};

use syn::{BinOp, Expr, Lit, Token, UnOp};

struct Input(Expr, Converter);

struct Converter {
    modulo: Expr,
    config: Config,
}

#[derive(Default)]
struct Config {
    array_elem: Option<bool>,
    assign_left: Option<bool>,
    assign_right: Option<bool>,
    add_assign_left: Option<bool>,
    add_assign_right: Option<bool>,
    sub_assign_left: Option<bool>,
    sub_assign_right: Option<bool>,
    mul_assign_left: Option<bool>,
    mul_assign_right: Option<bool>,
    div_assign_left: Option<bool>,
    div_assign_right: Option<bool>,
}

macro_rules! configure {
    ($self:expr; $($expr:expr => $item:ident),* $(,)?) => {
        {
            $(match $expr {
                Expr::Lit(syn::ExprLit {
                    lit: Lit::Bool(syn::LitBool { value, .. }),
                    ..
                }) => {
                    assert!($self.$item.is_none());
                    $self.$item = Some(value);
                }
                other => $self.configure(other),
            })*
        }
    };
}

impl Config {
    fn configure(&mut self, expr: Expr) {
        match expr {
            Expr::Array(expr) => {
                for elem in expr.elems {
                    configure!(self; elem => array_elem);
                }
            }
            Expr::Assign(expr) => configure! {
                self;
                *expr.left => assign_left,
                *expr.right => assign_right,
            },
            Expr::AssignOp(expr) => match expr.op {
                BinOp::AddEq(_) => configure! {
                    self;
                    *expr.left => add_assign_left,
                    *expr.right => add_assign_right,
                },
                BinOp::SubEq(_) => configure! {
                    self;
                    *expr.left => sub_assign_left,
                    *expr.right => sub_assign_right,
                },
                BinOp::MulEq(_) => configure! {
                    self;
                    *expr.left => mul_assign_left,
                    *expr.right => mul_assign_right,
                },
                BinOp::DivEq(_) => configure! {
                    self;
                    *expr.left => div_assign_left,
                    *expr.right => div_assign_right,
                },
                _ => panic!(),
            },
            _ => todo!(),
        }
    }
}

impl Parse for Input {
    fn parse(input: ParseStream) -> syn::parse::Result<Input> {
        let expr = input.parse()?;
        input.parse::<Token![mod]>()?;
        let modulo = input.parse()?;
        let mut config = Config::default();
        if !input.is_empty() {
            input.parse::<Token![;]>()?;
            config.configure(input.parse()?);
        }
        Ok(Input(expr, Converter { modulo, config }))
    }
}

pub fn modulo(tokens: TokenStream) -> TokenStream {
    use quote::ToTokens;
    let Input(mut expr, converter) = syn::parse2(tokens).unwrap();
    converter.convert(&mut expr);
    expr.to_token_stream()
}

trait Convert {
    fn convert(&self, expr: &mut Expr);
}

impl Convert for Converter {
    fn convert(&self, expr: &mut Expr) {
        let Converter { config, modulo } = self;
        match expr {
            Expr::Lit(_) => {}
            Expr::Path(_) => {}
            Expr::Assign(expr) => {
                if config.assign_left.unwrap_or(true) {
                    self.convert(&mut expr.left);
                }
                if config.assign_right.unwrap_or(true) {
                    self.convert(&mut expr.right);
                }
            }
            Expr::Unary(syn::ExprUnary { expr, op, .. }) => {
                self.convert(expr);
                *expr = match op {
                    UnOp::Neg(_) => syn::parse_quote! {
                        modulo_internals::sub(0, #expr, #modulo)
                    },
                    _ => todo!(),
                }
            }
            Expr::Binary(syn::ExprBinary {
                left, op, right, ..
            }) => {
                self.convert(left);
                self.convert(right);
                *expr = match op {
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
                *expr = match op {
                    BinOp::AddEq(_) => {
                        if config.add_assign_left.unwrap_or(true) {
                            self.convert(left);
                        }
                        if config.add_assign_right.unwrap_or(true) {
                            self.convert(right);
                        }
                        syn::parse_quote!(modulo_internals::add_assign(#right, &mut #left, #modulo))
                    }
                    BinOp::SubEq(_) => {
                        if config.sub_assign_left.unwrap_or(true) {
                            self.convert(left);
                        }
                        if config.sub_assign_right.unwrap_or(true) {
                            self.convert(right);
                        }
                        syn::parse_quote!(modulo_internals::sub_assign(#right, &mut #left, #modulo))
                    }
                    BinOp::MulEq(_) => {
                        if config.mul_assign_left.unwrap_or(true) {
                            self.convert(left);
                        }
                        if config.mul_assign_right.unwrap_or(true) {
                            self.convert(right);
                        }
                        syn::parse_quote!(modulo_internals::mul_assign(#right, &mut #left, #modulo))
                    }
                    BinOp::DivEq(_) => {
                        if config.div_assign_left.unwrap_or(true) {
                            self.convert(left);
                        }
                        if config.div_assign_right.unwrap_or(true) {
                            self.convert(right);
                        }
                        syn::parse_quote!(modulo_internals::div_assign(#right, &mut #left, #modulo))
                    }
                    _ => todo!(),
                }
            }
            Expr::Array(expr) => {
                if config.array_elem.unwrap_or(true) {
                    for elem in &mut expr.elems {
                        self.convert(elem);
                    }
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
