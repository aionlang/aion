//! The banner: the logo, the version and where to look for help.  `aion` with no arguments and no project to run prints it to
//! the standard output; a command `aion` does not know prints it to the standard error instead of an error message.

use std::io::IsTerminal;

/// The logo from `web/public/assets/aion_banner.svg`, drawn with block characters.  The blank row is the gap in the hourglass "A".
const LOGO: [&str; 8] = [
    "▀████████████▀",
    "  ██████████▀",
    "   ▀███████",
    "",
    "    ██████▄    ███  ▄███████████▄ █████▄   ███",
    "   ███  ▀██▄   ███  ███▀     ████ ███████▄ ███",
    " ▄███▄▄▄▄███▄  ███  ███      ████ ████ ▀██████",
    "▄███▀▀▀▀▀▀███▄ ███  ▀███████████▀ ████   ▀████",
];

/// How many columns of the logo are the "A"; the rest is "ion".
const A_WIDTH: usize = 15;

const BLUE: &str = "\x1b[38;2;67;119;223m";
const LIGHT: &str = "\x1b[38;2;129;163;232m";
const RESET: &str = "\x1b[0m";

const DOCS: &str = "https://aionlang.github.io/documentation";

/// The banner text.  `color` adds ANSI colours.
pub fn render(color: bool) -> String {
    let paint = |style: &str, text: &str| if color { format!("{style}{text}{RESET}") } else { text.to_string() };

    let mut out = String::from("\n");
    for row in LOGO {
        let split = row.char_indices().nth(A_WIDTH).map_or(row.len(), |(at, _)| at);
        let (a, ion) = row.split_at(split);
        out += &format!("  {}{}\n", paint(BLUE, a), paint(LIGHT, ion));
    }
    out += &format!("\n  {}\n", paint(BLUE, &format!("aion v{}", env!("CARGO_PKG_VERSION"))));
    out += &format!("\n  Run {} for usage, or read the docs: {}\n\n", paint(LIGHT, "aion --help"), paint(LIGHT, DOCS));
    out
}

#[derive(Clone, Copy)]
pub enum Stream {
    Stdout,
    Stderr,
}

/// Print the banner, coloured only on a terminal.
pub fn show(stream: Stream) {
    let is_terminal = match stream {
        Stream::Stdout => std::io::stdout().is_terminal(),
        Stream::Stderr => std::io::stderr().is_terminal(),
    };
    let color = is_terminal
        && std::env::var_os("NO_COLOR").is_none_or(|v| v.is_empty())
        && std::env::var("TERM").map_or(true, |t| t != "dumb")
        && enable_ansi(stream);
    match stream {
        Stream::Stdout => print!("{}", render(color)),
        Stream::Stderr => eprint!("{}", render(color)),
    }
}

/// The Windows console shows colour codes only once it is asked to; elsewhere they always work.
#[cfg(not(windows))]
fn enable_ansi(_stream: Stream) -> bool {
    true
}

#[cfg(windows)]
fn enable_ansi(stream: Stream) -> bool {
    use std::ffi::c_void;
    unsafe extern "system" {
        fn GetStdHandle(which: u32) -> *mut c_void;
        fn GetConsoleMode(handle: *mut c_void, mode: *mut u32) -> i32;
        fn SetConsoleMode(handle: *mut c_void, mode: u32) -> i32;
    }
    const STD_OUTPUT_HANDLE: u32 = -11i32 as u32;
    const STD_ERROR_HANDLE: u32 = -12i32 as u32;
    const ENABLE_VIRTUAL_TERMINAL_PROCESSING: u32 = 0x0004;
    let which = match stream {
        Stream::Stdout => STD_OUTPUT_HANDLE,
        Stream::Stderr => STD_ERROR_HANDLE,
    };
    // SAFETY: plain Win32 calls on a standard handle; a failure just means no colour.
    unsafe {
        let handle = GetStdHandle(which);
        let mut mode = 0;
        GetConsoleMode(handle, &mut mode) != 0 && SetConsoleMode(handle, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING) != 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_banner_names_the_version_and_where_to_look() {
        let plain = render(false);
        assert!(plain.contains(&format!("aion v{}", env!("CARGO_PKG_VERSION"))));
        assert!(plain.contains("aion --help") && plain.contains(DOCS));
        assert!(!plain.contains('\x1b'));
    }

    #[test]
    fn the_a_and_the_letters_have_their_own_blue() {
        let colored = render(true);
        assert!(colored.contains(BLUE) && colored.contains(LIGHT));
        // taking the colour away leaves the same text
        let stripped = colored.replace(BLUE, "").replace(LIGHT, "").replace(RESET, "");
        assert_eq!(stripped, render(false));
    }

    #[test]
    fn every_row_of_the_a_stays_left_of_the_letters() {
        for row in LOGO {
            assert!(row.chars().count() <= A_WIDTH || row.chars().nth(A_WIDTH - 1) == Some(' '), "`{row}` runs into the \"i\"");
        }
    }
}
