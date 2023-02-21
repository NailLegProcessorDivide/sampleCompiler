use std::env;

use source_ast::Prog;

mod block_structure;
mod compile_function;
mod const_prop;
mod source_ast;
mod tokens;
mod type_check;
mod x86;
mod unnest_exp;
mod front_end;
mod interp;
mod compile;

fn chop_file_name(filename: &str) -> &str
{
    filename.split(".expl").next().unwrap()
}

fn print_usage() {
    println!("usage:");
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let str_args : Vec<&str> = args.iter().map(|arg| arg.as_str()).collect();
    let mut arg_slice : &[&str] = &str_args[1..];
    let mut opt_filename : Option<&str> = None;
    let mut to_interp : bool = false;
    while arg_slice.len() != 0 {
        match arg_slice {
            [fname] => {
                opt_filename = Some(fname);
                arg_slice = &[];
            }
            ["--interp", args @ ..] => {
                to_interp = true;
                arg_slice = args;
            }
            [arg, ..] => panic!("unknown argument \"{}\"", *arg),
            [] => panic!("unreachable")
        }
    }
    let filename = match opt_filename {
        Some(name) => name,
        None => panic!("no file name given")
    };
    let input_filename = filename;
    let chopped_filename = chop_file_name(input_filename);
    let output_filename: String = format!("{}{}", chopped_filename, ".s");

    let mut program : Prog = front_end::front_end(input_filename, true);

    if to_interp {
        interp::interp(&program);
    }
    else {
        compile::compile(&mut program, &output_filename);
    }

    /*/
    */
    Ok(())
}
