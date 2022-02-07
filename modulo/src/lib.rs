use proc_macro::TokenStream;

#[proc_macro]
pub fn modulo(items: TokenStream) -> TokenStream {
    modulo_internals::modulo(items.into()).into()
}
