# Security policy

## Supported versions

Aion is in beta; only the latest release and the `main` branch get fixes.

## Reporting a vulnerability

Please do not open a public issue for a security problem.  Use the private reporting of GitHub:
**Security → Report a vulnerability** on <https://github.com/aionlang/aion>.  If that is not available, open an issue that
asks for a private contact and does not describe the problem.

Include what you found, how to reproduce it (a small program usually is enough) and what you think an attacker could do.
You can expect an answer within a week; we will tell you when it is fixed and credit you if you want.

## What counts

Problems in the compiler, the C runtime and the standard library that a program could not avoid, such as memory
corruption in `runtime/`, a wrong bounds check, or a weakness in `crypto`, TLS handling in `net`, or the archive and XML
readers when they handle data from outside.  A program that does something harmful on purpose with the access it already
has (it can run commands with `os`, for example) is not a vulnerability.

The `crypto` module wraps OpenSSL and has not been audited; do not rely on it for anything that needs an audited
implementation.
