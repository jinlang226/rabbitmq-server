#!/usr/bin/env bash
set -euo pipefail

cases=(
  feature-flag-enable
  plugins-set
  queue-rebalance
  await-online-quorum-plus-one
  publish-consume
  management-sequence
  plugin-toggle
  rebalance-loop
)

for tc in "${cases[@]}"; do
  echo "=== Running ${tc} ==="
  ./scripts/run_app_trace_case.sh "$tc"
  echo
  sleep 1
done

echo "All trace test cases finished."
