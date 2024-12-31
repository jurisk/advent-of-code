# Install toolchain

```shell
rustup update
rustup toolchain install nightly
rustup default nightly
```

# Maintain workspace

```shell
cargo update
cargo check --workspace
cargo install --locked cargo-udeps
cargo +nightly udeps
cargo install --locked cargo-outdated
cargo outdated --workspace
```

[Cargo outdated](https://crates.io/crates/cargo-outdated) depends on [#325](https://github.com/kbknapp/cargo-outdated/issues/325).

# Linting

```shell
cargo clippy
cargo clippy --all-targets --all-features -- -D warnings
cargo clippy -- -W clippy::pedantic
cargo clippy --fix -- -W clippy::pedantic
cargo clippy -- -W clippy::pedantic -A clippy::redundant_else
cargo clippy --all-targets --all-features -- -W clippy::pedantic
```

# Formatting

```shell
cargo fmt
```

# Building 

```shell
cargo build --release
```

# Running

```shell
cargo run --bin solution_<XY>
```

# Testing

```shell
cargo nextest run
cargo test
```

# Profiling

```shell
brew install cargo-instruments
xcode-select --version
cargo instruments --list-templates
cargo instruments --bin solution_<XY> --template 'CPU Profiler'
```

```shell
cargo install flamegraph
sudo cargo flamegraph --bin=solution_<XY>
```

```shell
time cargo run --release --bin=solution_<XY>
```

# Benchmarking

Note - I was considering to use Criterion, but we have the functions to be profiled in `bin` and not in `lib`.

```shell
brew install hyperfine
cargo build --release
hyperfine --shell=none --warmup 20 'target/release/solution_<XY>'
```
