use std::fs::File;
use std::io::Read;

use crate::source_ast::{parse_program, Prog};
use crate::tokens::{lex, print_token_list};
use crate::type_check::type_prog;

pub fn front_end(filename: &str, debug: bool) -> Result<Prog, String> {
    let mut file = File::open(filename).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let toks = lex(&contents[..], 0, 1)?;

    if debug {
        print_token_list(&toks);
    }
    let mut ast = parse_program(&toks)?;
    match type_prog(&mut ast) {
        Some(err) => Err(err),
        None => Ok(ast),
    }
}
