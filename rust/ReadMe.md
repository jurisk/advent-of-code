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
cargo clippy  -- -W clippy::pedantic -A clippy::cast_sign_loss  -A clippy::cast_precision_loss -A clippy::cast_possible_truncation -A clippy::cast_possible_wrap -A clippy::redundant_else -A clippy::naive_bytecount
```

# Formatting

```shell
cargo fmt
```

# Building 

```shell
cargo build --release
```

# Testing

```shell
cargo test
```

# Profiling

```shell
brew install cargo-instruments
xcode-select --version
cargo instruments --list-templates
cargo instruments --bin solution_<XY> --template 'CPU Profiler'
```
