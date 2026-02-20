#!/usr/bin/env bash
# This script is superseded by a GitHub Actions workflow for CI/CD.
# Run `npm run build` to produce a production build in the dist/ directory.
npm run build || exit

echo "Build complete. Output is in the dist/ directory."
