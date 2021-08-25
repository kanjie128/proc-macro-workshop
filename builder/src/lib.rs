use core::panic;

use proc_macro::TokenStream;
use quote::quote;
// use quote::quote;
use syn::{self, parse_macro_input, DeriveInput};

// builder pattern: https://rust-lang-nursery.github.io/api-guidelines/type-safety.html#c-builder

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    // let ast = syn::parse(input).unwrap();
    let ast = parse_macro_input!(input as DeriveInput);
    // eprintln!("ast: {:#?}", ast);
    impl_builder_derive_macro(&ast)
}

fn impl_builder_derive_macro(ast: &syn::DeriveInput) -> TokenStream {
    //origin struct eg. Command
    let ori_ident = &ast.ident;
    //builder struct eg. CommandBuiler
    let builder_name = format!("{}Builder", ori_ident);
    let builder_ident = syn::Ident::new(&builder_name, ori_ident.span());

    // fist parse all fields of origin struct
    // ```
    // struct Command {
    //   excutable: String,
    //   alias: Option<String>
    //   env: Vec<String>,
    //   args: Vec<String>,
    // }
    // ```
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        panic!("");
    };

    // convert origin struct fields to builder struct fields
    // if field's type is not Option, we wrap it to Option.
    // eg. excutable
    // ```
    // struct CommandBuilder {
    //   excutable: Option<String>,
    //   alias: Option<String>
    //   env: Option<Vec<String>>,
    //   args: Option<vec<String>>,
    // }
    // ```
    let builder_fields = fields.iter().map(|f| {
        let field_name = &f.ident;
        let typ = &f.ty;
        if inspect_inner_type("Option", typ).is_some() {
            quote! {#field_name: #typ}
        } else {
            quote! {#field_name: std::option::Option<#typ>}
        }
    });

    // construct methods for CommandBuilder like:
    // fn excutable(&mut self, excutable: String) -> &mut Self {
    //   self.excutable = excutable;
    //   self
    // }
    let methods = fields.iter().map(|f| {
        let field_name = f.ident.as_ref().unwrap();
        let typ = &f.ty;

        let field_method = if let Some(inner_type) = inspect_inner_type("Option", typ) {
            quote! {
                pub fn #field_name(&mut self, value: #inner_type) -> &mut Self {
                    self.#field_name = Some(value);
                    self
                }
            }
        } else {
            quote! {
               pub fn #field_name(&mut self, value: #typ) -> &mut Self {
                    self.#field_name = Some(value);
                    self
                }
            }
        };
        // deal with field's attribute #[builder(..)].
        // eg:
        // ```
        // struct Command {
        //   excutable: String,
        //   alias: Option<String>
        //   #[builder(each = "env")]
        //   env: Vec<String>,
        //   #[builder(each = "arg")]
        //   args: Vec<String>,
        // }
        // ```
        for attr in &f.attrs {
            // syn::Ident implete PartialEq of str
            if attr.path.segments.len() != 1 || attr.path.segments[0].ident != "builder" {
                continue;
            }
            match attr.parse_meta() {
                Ok(syn::Meta::List(mut list)) => {
                    assert!(list.path.is_ident("builder"));
                    if list.nested.len() != 1 {
                        panic!("attr builder should has only 1 meta");
                    }
                    match list.nested.pop() {
                        Some(v) => {
                            if let syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) =
                                v.into_value()
                            {
                                if !nv.path.is_ident("each") {
                                    // panic!("attr builder should only contain meta `each`")
                                    return syn::Error::new_spanned(list, "expected `builder(each = \"...\")`").to_compile_error();
                                }
                                match nv.lit {
                                    syn::Lit::Str(s) => {
                                        let name = syn::Ident::new(&s.value(), s.span());
                                        let inner_type =
                                            inspect_inner_type("Vec", &f.ty).unwrap();
                                        let mut  f= quote! {
                                            pub fn #name(&mut self, #name:#inner_type) -> &mut Self {
                                                match self.#field_name {
                                                    Some(ref mut v) => v.push(#name),
                                                    None =>{
                                                        self.#field_name = Some(vec![#name]);
                                                    }
                                                }
                                                self
                                            }
                                        };

                                        // if fiels method and attr meta method has same name, we use meta's
                                        if &name != field_name {
                                            f = quote!{
                                                #f
                                                #field_method
                                            }
                                        }
                                        return f;
                                    }
                                    lit => panic!(
                                        "attr meta should be string, but found {:?}",
                                        lit
                                    ),
                                }
                            }
                        }
                        None => {
                            panic!("attr not correct");
                        }
                    }
                }
                Ok(_meta) => {
                    // maybe #[buider = "each"] ?
                    panic!("attr format not correct {:?}", _meta);
                }
                Err(e) => {
                    panic!("{}", e);
                }
            }
        }
        field_method
    });

    // builder's build method
    let build_fn = fields.iter().map(|f| {
        let field_name = &f.ident;
        let typ = &f.ty;
        if inspect_inner_type("Option", typ).is_some() {
            quote! {
                #field_name: self.#field_name.clone()
            }
        } else if inspect_inner_type("Vec", typ).is_some() {
            quote!{
                #field_name: self.#field_name.clone().unwrap_or_default()
            }
        }else{
            quote! {
                #field_name: self.#field_name.clone().ok_or(concat!(stringify!(#field_name)," is not set"))?
            }
        }
    });

    // origin struct's builder method
    let builder_field_default = fields.iter().map(|f| {
        let field_name = &f.ident;
        quote! {
            #field_name: None
        }
    });

    // construct our CommandBuilder
    let expanded = quote! {
        pub struct #builder_ident {
            // repeat all fields
            #(#builder_fields,)*
        }
        impl #builder_ident {
            // repeat all methods
            #(#methods)*

           pub fn build(&self) -> std::result::Result<#ori_ident, std::boxed::Box::<dyn std::error::Error>>{
                Ok(#ori_ident {
                    #(#build_fn,)*
                })
            }
        }
        impl #ori_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#builder_field_default,)*
                }
            }
        }
    };

    expanded.into()
}

fn inspect_inner_type<'a>(ty_name: &str, ty: &'a syn::Type) -> std::option::Option<&'a syn::Type> {
    if let syn::Type::Path(ref path) = ty {
        if path.path.segments.len() != 1 || path.path.segments[0].ident != ty_name {
            return None;
        }
        if let syn::PathArguments::AngleBracketed(ref arg) = path.path.segments[0].arguments {
            if arg.args.len() != 1 {
                return None;
            }
            if let syn::GenericArgument::Type(ref t) = arg.args[0] {
                return Some(t);
            }
        }
    }
    None
}
