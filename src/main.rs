use std::io;
mod repl;

fn main() -> Result<(), std::io::Error> {
    repl::run_repl(&mut io::stdin(), &mut io::stdout())?;
    Ok(())
}
