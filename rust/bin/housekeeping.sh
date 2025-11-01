#!/usr/bin/env sh

echo "🔍 Running code formatting..."
cargo fmt --quiet

echo "🔍 Running clippy checks..."
cargo clippy --workspace --all-targets --all-features -- \
  -W clippy::pedantic \
  -W clippy::style \
  -W clippy::allow_attributes
