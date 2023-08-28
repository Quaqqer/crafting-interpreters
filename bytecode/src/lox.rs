use clap::Parser;

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

pub fn repl() {}

pub fn run_file(path: &str) {}
