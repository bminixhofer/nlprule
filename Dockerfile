# syntax=docker/dockerfile:1

# Builds the nlprule workspace with the same feature set as the documented
# build (compile/bin/zh/ja): the `compile`, `test`, `run`, `check_server`,
# `bench`, `debug_filters`, `test_disambiguation` and `opennlp_to_chunker`
# binaries are installed on PATH in the runtime image.
FROM rust:1-bookworm AS builder
WORKDIR /build
COPY Cargo.toml Cargo.lock ./
COPY nlprule ./nlprule
COPY build ./build
RUN cargo build --release --features "compile bin zh ja"

# The rule binaries under storage/ total ~5.6 GB for all 35 languages, so they
# are NOT baked into the image. Mount the host storage/ at /storage:
#   docker build -t nlprule-all .
#   docker run -i --rm -v "$PWD/storage:/storage:ro" nlprule-all en
FROM debian:bookworm-slim AS runtime
RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates \
    && rm -rf /var/lib/apt/lists/*
COPY --from=builder /build/target/release/compile \
    /build/target/release/test \
    /build/target/release/run \
    /build/target/release/check_server \
    /build/target/release/bench \
    /build/target/release/debug_filters \
    /build/target/release/test_disambiguation \
    /build/target/release/opennlp_to_chunker \
    /usr/local/bin/
COPY docker/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh
WORKDIR /work
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
