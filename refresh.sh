#!/usr/bin/env bash
#
# Refresh the dynamic pages after editing R/ code.
#
# `freeze: auto` hashes each .qmd, not the sourced R/*.R files, so edits to R/
# don't invalidate the cached output. This clears the freeze cache for the two
# dynamic pages and re-renders (or previews) them.
#
# Usage:
#   ./refresh.sh            # clear cache + render packages.qmd and vignettes.qmd
#   ./refresh.sh preview    # clear cache + start a watching preview of the site
#
set -euo pipefail
cd "$(dirname "$0")"

# Authenticate the GitHub API calls (higher rate limit) if a token is available.
: "${GITHUB_PAT:=$(gh auth token 2>/dev/null || true)}"
export GITHUB_PAT

rm -rf _freeze/packages _freeze/vignettes

if [ "${1:-render}" = "preview" ]; then
  quarto preview
else
  quarto render packages.qmd vignettes.qmd
fi
