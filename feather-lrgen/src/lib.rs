use featherparse::{LRAction, Symbol};
use proc_macro::TokenStream;
use quote::quote;

#[proc_macro]
pub fn gen_parse_tables(_input: TokenStream) -> TokenStream {
    let (action_table, goto_table) = featherparse::build_parsing_tables();
    let action_tokens = action_table.into_iter()
            .map(|(k, v)| {
                let state = k.state;
                let token = k.token;
                let action = match v {
                    LRAction::Shift(state) => {
                        quote! { featherparse::LRAction::Shift(#state) }
                    }
                    LRAction::Reduce(prod) => {
                        let name = prod.name;
                        let symbols = prod.symbols.iter().map(|symbol| {
                            match symbol {
                                Symbol::Epsilon() => quote! { Symbol::Epsilon },
                                Symbol::Token(token) => quote! { Symbol::Token(#token.to_string()) },
                                Symbol::Expression(expr) => quote! { Symbol::Expression(#expr.to_string()) },
                            }
                        }).collect::<Vec<_>>();
                        quote! {
                            featherparse::LRAction::Reduce(featherparse::Production {
                                name: #name.to_string(),
                                symbols: Vec::from([#(#symbols),*])
                            })
                        }
                    }
                    LRAction::Accept() => quote! { LRAction::Accept() }
                };
                quote! {
                    (featherparse::ActionTableKey { state: #state, token: #token.to_string() }, #action)
                }
            });
    let goto_tokens = goto_table.into_iter()
            .map(|(k, v)| {
                let state = k.state;
                let symbol = k.symbol;
                let goto_state = v;
                quote! {
                    (featherparse::GotoTableKey { state: #state, symbol: #symbol.to_string() }, #goto_state)
                }
            });

    let output = quote! {
        (
            HashMap::from([
                #(#action_tokens),*
            ]),
            HashMap::from([
                #(#goto_tokens),*
            ])
        )
    };

    return output.into();
}
