# Aion in a box: compiler + clang + the libraries the standard library links against.
# No Rust, cargo or clang needed on the host.
#
#   docker build -t aion .
#   docker run --rm -v "$PWD":/work aion examples/hello.aion
#
# See docs/docker.md.

# ---- build the compiler ------------------------------------------------------
FROM rust:1-slim-trixie AS build
WORKDIR /src
COPY Cargo.toml Cargo.lock build.rs ./
COPY src ./src
COPY runtime ./runtime
COPY stdlib ./stdlib
RUN cargo build --release --locked

# ---- runtime image -----------------------------------------------------------
FROM debian:trixie-slim
# clang compiles the generated IR; the -dev packages are what `sqlite` and `zlib`
# link against; libssl is loaded at run time by `net`/`http` for TLS; git is
# needed for `// dep:` and `aion add` git dependencies.
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
        clang libc6-dev libsqlite3-dev zlib1g-dev libssl3t64 ca-certificates git \
 && rm -rf /var/lib/apt/lists/*

COPY --from=build /src/target/release/aion /usr/local/bin/aion
COPY examples /opt/aion/examples

# Compiled programs and cloned dependencies; mount a volume here to keep them.
ENV AION_CACHE=/cache
VOLUME /cache

WORKDIR /work
ENTRYPOINT ["aion"]
CMD ["--help"]
