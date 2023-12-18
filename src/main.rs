use std::{
    env::args_os,
    ffi::{OsStr, OsString},
    fs::read_to_string,
    io::{stdin, stdout, BufRead, Result, Write},
    process::exit,
};

use lexer::tokenize_with_text;

mod lexer;

fn main() -> Result<()> {
    let args: Vec<OsString> = args_os().collect();

    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => {
            println!("Usage: rulox [script]");
            exit(64);
        }
    }
}

fn run(source: &str) {
    for (kind, text) in tokenize_with_text(source) {
        println!("{kind:?} {text:?}");
    }
}

fn run_file(path: &OsStr) -> Result<()> {
    run(&read_to_string(path)?);
    Ok(())
}

fn run_prompt() -> Result<()> {
    let stdin = stdin();
    let mut stdout = stdout();

    let mut lines = stdin.lock().lines();

    loop {
        write!(stdout, "> ")?;
        stdout.flush()?;

        if let Some(line) = lines.next() {
            run(&line?);
        } else {
            break;
        }
    }

    Ok(())
}
