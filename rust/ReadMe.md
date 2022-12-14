```shell
rustup update
rustup toolchain install nightly
rustup default nightly
cargo clippy
cargo clippy --all-targets --all-features -- -D warnings
cargo clippy -- -W clippy::pedantic
cargo clippy --fix -- -W clippy::pedantic
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
