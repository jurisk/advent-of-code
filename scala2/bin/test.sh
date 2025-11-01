#!/bin/bash

# Test script - runs all tests
# Similar to cargo test for Scala 3

set -e

echo "🧪 Running Scala tests..."

echo "🔧 Compiling..."
sbt compile
if [ $? -ne 0 ]; then
    echo "❌ Compilation failed"
    exit 1
fi
echo "✅ Compilation successful"

echo "🧪 Running tests..."
sbt test
if [ $? -eq 0 ]; then
    echo "✅ All tests passed!"
else
    echo "❌ Some tests failed"
    exit 1
fi