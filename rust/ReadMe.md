```shell
rustup update
rustup toolchain install nightly
rustup default nightly
cargo clippy
cargo clippy --all-targets --all-features -- -D warnings
cargo clippy -- -W clippy::pedantic
cargo clippy --fix -- -W clippy::pedantic
cargo clippy  -- -W clippy::pedantic -A clippy::cast_sign_loss  -A clippy::cast_precision_loss -A clippy::cast_possible_truncation -A clippy::cast_possible_wrap -A clippy::redundant_else -A clippy::naive_bytecount
cargo fmt
cargo build --release
cargo test
```

```shell
brew install cargo-instruments
xcode-select --version
cargo instruments --list-templates
cargo instruments --bin solution_<XY> --template 'CPU Profiler'
```
