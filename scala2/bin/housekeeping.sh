#!/bin/bash

# Housekeeping script for Scala2 project
# Runs code formatting and linting

set -e

echo "🧹 Running housekeeping tasks..."

echo "📝 Running scalafmt..."
sbt scalafmtAll

echo "🔧 Running scalafix..."
sbt scalafixAll

echo "✅ Housekeeping completed successfully!"