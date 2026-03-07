#!/usr/bin/env bash
# Weekly automated update for state-elect-law-db
# Runs script.R, then commits and pushes any changes to GitHub.
# Usage: bash run_weekly.sh
#        (or let cron call it — see README for setup)

set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_FILE="$REPO_DIR/run_weekly.log"
DATE="$(date '+%Y-%m-%d %H:%M:%S')"

log() { echo "[$DATE] $*" | tee -a "$LOG_FILE"; }

log "=== Weekly run starting ==="
cd "$REPO_DIR"

# ── Run the R script ──────────────────────────────────────────────────────────
log "Running code/script.R..."
if Rscript -e "source('renv/activate.R'); source('code/script.R')" >> "$LOG_FILE" 2>&1; then
  log "R script completed successfully."
else
  log "ERROR: R script failed. Check log above for details."
  exit 1
fi

# ── Commit and push any changes ───────────────────────────────────────────────
if [[ -n "$(git status --porcelain)" ]]; then
  log "Changes detected — staging, committing, and pushing..."
  git add -A
  git commit -m "Weekly automated update – $(date '+%Y-%m-%d')"
  git push origin
  log "Push complete."
else
  log "No changes detected — nothing to commit."
fi

log "=== Weekly run finished ==="
