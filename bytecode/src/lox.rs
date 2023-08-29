use std::{io::Write, process::exit};

use clap::Parser;

use crate::vm::VM;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct CliArgs {
    file: Option<String>,
}

pub fn cli() {
    let args = CliArgs::parse();

    if let Some(f) = args.file {
        run_file(f.as_str());
    } else {
        repl();
    }
}

fn interpret(vm: &mut VM, s: &str) {
    let res = vm.interpret_str(s);
    match res {
        Ok(_) => todo!(),
        Err(e) => match e {
            crate::vm::Error::CompileError(msg) => {
                eprintln!("{}", msg);
                exit(65);
            }
            crate::vm::Error::DecodeError(msg) => {
                eprintln!("{}", msg);
                exit(70);
            }
        },
    };
}

pub fn repl() {
    let mut vm = VM::new();
    loop {
        let mut line = String::new();
        print!("> ");
        std::io::stdout().flush().expect("Could not flush");

        std::io::stdin()
            .read_line(&mut line)
            .expect("Could not read line from stdin");

        // Exit when pressing <C-d>
        if line == "" {
            println!("");
            return;
        }

        interpret(&mut vm, line.as_str());
    }
}

pub fn run_file(path: &str) {
    let mut vm = VM::new();
    let source = std::fs::read_to_string(path).unwrap();
    interpret(&mut vm, &source);
}
