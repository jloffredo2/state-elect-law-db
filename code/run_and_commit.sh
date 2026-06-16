#!/bin/bash

REPO="/Users/josephloffredo/Desktop/GitHub/state-elect-law-db"
LOG="$REPO/logs/run.log"

mkdir -p "$REPO/logs"
exec >> "$LOG" 2>&1

echo ""
echo "=== $(date) ==="

# Load VRL credentials from ~/.Renviron
export VRL_EMAIL=$(grep '^VRL_EMAIL=' ~/.Renviron | cut -d= -f2 | tr -d '[:space:]')
export VRL_PASSWORD=$(grep '^VRL_PASSWORD=' ~/.Renviron | cut -d= -f2 | tr -d '[:space:]')

cd "$REPO"

/usr/local/bin/Rscript code/script.R
if [ $? -ne 0 ]; then
    echo "Rscript failed. Aborting commit."
    exit 1
fi

echo "Script completed. Committing output..."

/usr/local/bin/git add output/

/usr/local/bin/git diff --cached --quiet && echo "No changes to commit." && exit 0

/usr/local/bin/git commit -m "Automated update – $(date '+%Y-%m-%d')"
/usr/local/bin/git push

echo "Done."
