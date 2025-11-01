#!/bin/bash

# Housekeeping script - runs Scalafmt and Scalafix
# Similar to cargo clippy for Scala 3

set -e

echo "🧹 Running Scala housekeeping..."

echo "📝 Running Scalafmt (formatter)..."
sbt scalafmtAll
if [ $? -eq 0 ]; then
    echo "✅ Scalafmt completed successfully"
else
    echo "❌ Scalafmt failed"
    exit 1
fi

echo "🔍 Running Scalafix (linter)..."
sbt "scalafixAll --check"
if [ $? -eq 0 ]; then
    echo "✅ Scalafix completed successfully - no issues found"
else
    echo "⚠️  Scalafix found issues. Running with auto-fix..."
    sbt scalafixAll
    if [ $? -eq 0 ]; then
        echo "✅ Scalafix auto-fixes applied successfully"
    else
        echo "❌ Scalafix failed to apply fixes"
        exit 1
    fi
fi

echo "🎉 Housekeeping completed successfully!"