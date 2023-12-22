use std::{
    env::args_os,
    ffi::{OsStr, OsString},
    fs::read_to_string,
    io::{stdin, stdout, BufRead, Result, Write},
    process::exit,
};

use lexer::tokenize_with_text;

use crate::lexer::TokenKind;

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

fn report(line: usize, message: &str) {
    eprintln!("[line {line}] Error: {message}");
}

fn run(source: &str) -> bool {
    let mut line = 1;
    let mut has_error = false;

    for (kind, text) in tokenize_with_text(source) {
        if kind.is_multiline_token() {
            line += text.bytes().filter(|c| *c == b'\n').count();
        }

        if kind.is_trivia_token() {
            continue;
        }

        match kind {
            TokenKind::NUMBER => {
                println!(
                    "NUMBER {text} {text}{}",
                    if text.contains('.') { "" } else { ".0" }
                );
            }
            TokenKind::STRING { terminated: true } => {
                println!("STRING {text} {}", &text[1..text.len() - 1]);
            }
            TokenKind::STRING { terminated: false } => {
                report(line, "Unterminated string.");
                has_error = true;
            }
            TokenKind::UNKNOWN => {
                report(line, "Unexpected character.");
                has_error = true;
            }
            _ => println!("{kind:?} {text} null"),
        }
    }

    !has_error
}

fn run_file(path: &OsStr) -> Result<()> {
    if !run(&read_to_string(path)?) {
        exit(65);
    }

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
