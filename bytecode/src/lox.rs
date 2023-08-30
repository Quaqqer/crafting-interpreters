use std::{io::Write, process};

use clap::Parser;

use crate::vm::{self, VM};

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

fn interpret(vm: &mut VM, s: &str, repl: bool) {
    let res = vm.interpret_str(s, repl);
    if let Err(e) = res {
        eprintln!("{}", e);
        if !repl {
            match e {
                vm::Error::Compiler(_) => process::exit(65),
                _ => process::exit(70),
            }
        }
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
        line = line.trim_end().to_string();

        // Exit when pressing <C-d>
        if line == "" {
            println!("");
            return;
        }

        interpret(&mut vm, line.as_str(), true);
    }
}

pub fn run_file(path: &str) {
    let mut vm = VM::new();
    let source = std::fs::read_to_string(path).unwrap();
    interpret(&mut vm, &source, false);
}
