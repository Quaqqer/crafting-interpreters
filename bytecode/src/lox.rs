//! The human interface for Lox

use std::{io::Write, process};

use clap::Parser;

use crate::vm::{self, DefaultVMIO, VM};

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct CliArgs {
    file: Option<String>,
}

/// Run the CLI of the lox interpreter
pub fn cli() {
    let args = CliArgs::parse();

    if let Some(f) = args.file {
        run_file(f.as_str());
    } else {
        repl();
    }
}

fn interpret(vm: &mut VM, s: &str, repl: bool) {
    let res = vm.interpret_str(s);
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

/// Start a repl
///
/// Exits on `<C-d>`
pub fn repl() {
    let mut io = DefaultVMIO::new();
    let mut vm = VM::new(&mut io);
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

/// Run a file from a path
pub fn run_file(path: &str) {
    let mut io = DefaultVMIO::new();
    let mut vm = VM::new(&mut io);
    let source = std::fs::read_to_string(path).unwrap();
    interpret(&mut vm, &source, false);
}
