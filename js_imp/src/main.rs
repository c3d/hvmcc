use clap::Parser;
use js_imp::{js_to_imp};
use std::path::Path;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about=None)]
struct Arg {
  #[arg(short, long)]
  file: String,
}

fn main() {
  let args = Arg::parse();
  let file = Path::new(&args.file);
  let prog = js_to_imp(file);
  println!("{}", prog.unwrap())
}
