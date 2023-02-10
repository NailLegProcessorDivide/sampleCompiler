use std::env;
use std::fs::File;
use std::io::{Write};
use std::collections::{HashSet, HashMap};

use source_ast::Prog;

mod block_structure;
mod compile_function;
mod const_prop;
mod source_ast;
mod tokens;
mod x86;
mod unnest_exp;
mod front_end;

fn chop_file_name(filename: &str) -> &str
{
    filename.split(".expl").next().unwrap()
}

fn print_usage() {
    println!("usage:");
}

fn write_header(file: &mut File) {
    write!(file, "[section .text align=16]\n").ok();
    write!(file, "global main\n\n").ok();
    write!(file, "extern signal_error\n").ok();
    write!(file, "extern input\n").ok();
    write!(file, "extern output\n").ok();
    write!(file, "extern allocate1\n").ok();
    write!(file, "extern allocate2\n").ok();
    write!(file, "extern allocate3\n").ok();
    write!(file, "extern allocate4\n").ok();
    write!(file, "extern allocate5\n").ok();
    write!(file, "extern allocate6\n").ok();
    write!(file, "extern allocate7\n\n").ok();
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        print_usage();
        return Ok(());
    }
    let input_filename = args.into_iter().nth(1).unwrap();
    let chopped_filename = chop_file_name(&input_filename[..]);
    let output_filename = format!("{}{}", chopped_filename, ".s");

    let program : Prog = front_end::front_end(&input_filename, true); 
    /*/
    let mut file = File::create(output_filename)?;
    write_header(&mut file);
    let _functions = compile_function::compile_fun(false, input_filename, HashSet::new(), );
    */
    Ok(())
}
