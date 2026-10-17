//! `aion fmt`: tidy a file's whitespace without changing a token of it.
//!
//! Tabs that start a line become four spaces, trailing spaces go, runs of blank lines shrink to one,
//! the file ends in exactly one line break, and line endings stay as the file has them.  The inside of
//! multi-line strings is never touched.

use crate::syntax::{Token, lex};

/// The formatted text, or why the file could not be read as Aion.
pub fn format(text: &str) -> Result<String, String> {
    let crlf = text.contains("\r\n");
    let src = text.replace("\r\n", "\n");
    let tokens = lex(0, &src, 0).map_err(|d| d.message)?;

    // Which line breaks sit inside a string literal?
    let strings: Vec<(usize, usize)> = tokens
        .iter()
        .filter(|t| matches!(t.kind, Token::Str | Token::RawStr))
        .map(|t| (t.span.start as usize, t.span.end as usize))
        .collect();
    let inside_string = |at: usize| strings.iter().any(|&(start, end)| start < at && at < end);

    let mut out = String::with_capacity(src.len());
    let mut offset = 0;
    let mut blank_run = 0;
    let mut start_is_code = true; // the line starts outside any string
    let lines: Vec<&str> = src.split('\n').collect();
    for (i, line) in lines.iter().enumerate() {
        let is_last = i + 1 == lines.len();
        let end = offset + line.len();
        let end_is_code = is_last || !inside_string(end);
        let mut text = (*line).to_string();
        let started_in_code = start_is_code;
        if started_in_code {
            let indent = text.len() - text.trim_start_matches([' ', '\t']).len();
            text = text[..indent].replace('\t', "    ") + &text[indent..];
        }
        if end_is_code {
            text.truncate(text.trim_end_matches([' ', '\t']).len());
        }
        (offset, start_is_code) = (end + 1, end_is_code);
        // a run of blank lines outside strings shrinks to one
        if text.is_empty() && started_in_code && end_is_code {
            blank_run += 1;
            if blank_run > 1 {
                continue;
            }
        } else {
            blank_run = 0;
        }
        out.push_str(&text);
        if !is_last {
            out.push('\n');
        }
    }
    let trimmed = out.trim_end_matches('\n').len();
    out.truncate(trimmed);
    if !out.is_empty() {
        out.push('\n');
    }
    Ok(if crlf { out.replace('\n', "\r\n") } else { out })
}

#[cfg(test)]
mod tests {
    use super::format;

    #[test]
    fn whitespace_is_tidied() {
        assert_eq!(format("fn f() {  \n\treturn 1\t\n}\n\n\n\nx := 2").unwrap(), "fn f() {\n    return 1\n}\n\nx := 2\n");
    }

    #[test]
    fn strings_keep_their_insides() {
        let src = "s := \"\"\"a  \n\tb  \n\n\n  c\"\"\"  \n";
        assert_eq!(format(src).unwrap(), "s := \"\"\"a  \n\tb  \n\n\n  c\"\"\"\n");
    }

    #[test]
    fn line_endings_are_kept() {
        assert_eq!(format("a := 1  \r\n\r\n\r\nb := 2").unwrap(), "a := 1\r\n\r\nb := 2\r\n");
    }

    #[test]
    fn unreadable_text_is_reported() {
        assert!(format("x := `").is_err());
    }

    #[test]
    fn formatting_twice_changes_nothing() {
        let once = format("if a {\n\t\tb()  \n}\n\n\n").unwrap();
        assert_eq!(format(&once).unwrap(), once);
    }
}
