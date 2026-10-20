//! The banner `aion` prints when it is given a command it does not know.

use std::io::IsTerminal;

/// The logo from `web/public/assets/aion_banner.svg`, drawn with block characters. The blank row is the gap in the hourglass "A".
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

/// May the banner use colours?  Not when `NO_COLOR` is set or stderr is not a terminal, and on Windows only in terminals known to
/// understand escape codes (the old console window prints them as text).
pub fn colors_enabled() -> bool {
    std::env::var_os("NO_COLOR").is_none()
        && std::io::stderr().is_terminal()
        && (!cfg!(windows) || std::env::var_os("WT_SESSION").is_some() || std::env::var_os("TERM").is_some())
}

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
