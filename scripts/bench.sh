#! /usr/bin/env bash
set -euo pipefail

OUTPUT="$(git branch --show-current)-$(git rev-parse --short HEAD).csv"
stack bench random:bench --ba "--csv=${OUTPUT} --small"
echo "Results are in $OUTPUT"
