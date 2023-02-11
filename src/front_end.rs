use std::fs::File;
use std::io::{Read};

use crate::source_ast::{Prog, parse_program};
use crate::tokens::{print_token_list, lex};

pub fn front_end (filename : &str, debug : bool) -> Prog {
    let mut file = File::open(filename).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let toks = lex(&contents[..], 0, 1);
    if debug {
        print_token_list(&toks);
    }
    let ast = parse_program(&toks);
    todo!()
}
