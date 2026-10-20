//! What `aion` prints when it is run with no arguments and there is no project to run: the logo, the version, what Aion
//! is, and the first things to try.  It goes to the standard output and is coloured only on a terminal.

use std::io::IsTerminal;

/// The letters A I O N, five rows high.  The "A" is the darker blue of the logo, the other letters the lighter one.
const LOGO: [(&str, &str); 5] = [
    (" ███  ", " █████   ███   █   █"),
    ("█   █ ", "   █    █   █  ██  █"),
    ("█████ ", "   █    █   █  █ █ █"),
    ("█   █ ", "   █    █   █  █  ██"),
    ("█   █ ", " █████   ███   █   █"),
];

const DARK: &str = "\x1b[38;2;67;119;223m";
const LIGHT: &str = "\x1b[38;2;129;163;232m";
const BOLD: &str = "\x1b[1m";
const DIM: &str = "\x1b[2m";
const RESET: &str = "\x1b[0m";

/// The welcome text.  `color` adds ANSI colours.
pub fn text(color: bool) -> String {
    let paint = |style: &str, s: &str| if color { format!("{style}{s}{RESET}") } else { s.to_string() };
    let mut out = String::from("\n");
    for (a, ion) in LOGO {
        out.push_str(&format!("  {}{}\n", paint(DARK, a), paint(LIGHT, ion)));
    }
    out.push_str(&format!("\n  {}  {}\n", paint(BOLD, &format!("Aion {}", env!("CARGO_PKG_VERSION"))), paint(DIM, "beta")));
    out.push_str(
        "\n  A statically typed language that reads like a script and compiles to a native program.\n\
         \n  Get started\n\
         \x20   aion hello.aion         compile and run a file\n\
         \x20   aion new myapp          create a project\n\
         \x20   aion repl               an interactive prompt\n\
         \x20   aion build hello.aion   make a standalone executable\n\
         \n  Run `aion --help` for all the commands and options.\n\
         \n  Documentation  https://aionlang.github.io/documentation\n\
         \x20 Source         https://github.com/aionlang/aion\n\n",
    );
    out
}

/// Print the welcome text to the standard output.
pub fn print() {
    let color = std::io::stdout().is_terminal()
        && std::env::var_os("NO_COLOR").is_none_or(|v| v.is_empty())
        && std::env::var("TERM").map_or(true, |t| t != "dumb")
        && enable_ansi();
    print!("{}", text(color));
}

/// The Windows console shows colour codes only once it is asked to; elsewhere they always work.
#[cfg(not(windows))]
fn enable_ansi() -> bool {
    true
}

#[cfg(windows)]
fn enable_ansi() -> bool {
    use std::ffi::c_void;
    unsafe extern "system" {
        fn GetStdHandle(which: u32) -> *mut c_void;
        fn GetConsoleMode(handle: *mut c_void, mode: *mut u32) -> i32;
        fn SetConsoleMode(handle: *mut c_void, mode: u32) -> i32;
    }
    const STD_OUTPUT_HANDLE: u32 = -11i32 as u32;
    const ENABLE_VIRTUAL_TERMINAL_PROCESSING: u32 = 0x0004;
    // SAFETY: plain Win32 calls on the standard output handle; a failure just means no colour.
    unsafe {
        let handle = GetStdHandle(STD_OUTPUT_HANDLE);
        let mut mode = 0;
        GetConsoleMode(handle, &mut mode) != 0 && SetConsoleMode(handle, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING) != 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_welcome_names_the_version_and_the_help() {
        let plain = text(false);
        assert!(plain.contains(&format!("Aion {}", env!("CARGO_PKG_VERSION"))));
        assert!(plain.contains("aion --help"));
        assert!(!plain.contains('\x1b'), "no colour codes unless asked for");
        assert!(text(true).contains('\x1b'));
    }
}
