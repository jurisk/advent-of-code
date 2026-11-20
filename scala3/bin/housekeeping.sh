#!/bin/bash

# Housekeeping script - runs Scalafmt and Scalafix
# Similar to cargo clippy for Scala 3

# --jvm-client: the native sbtn client swallows server error messages
SBT="sbt --jvm-client"

echo "🧹 Running Scala housekeeping..."

echo "📝 Running Scalafmt (formatter)..."
if $SBT scalafmtAll; then
    echo "✅ Scalafmt completed successfully"
else
    echo "❌ Scalafmt failed"
    exit 1
fi

echo "🔍 Running Scalafix (linter)..."
if $SBT "scalafixAll --check"; then
    echo "✅ Scalafix completed successfully - no issues found"
else
    echo "⚠️  Scalafix found issues. Running with auto-fix..."
    if $SBT scalafixAll; then
        echo "✅ Scalafix auto-fixes applied successfully"
    else
        echo "❌ Scalafix failed to apply fixes"
        exit 1
    fi
fi

echo "🎉 Housekeeping completed successfully!"