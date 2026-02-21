#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <testcase>"
  echo "Try: $0 list"
  exit 1
fi

TESTCASE="$1"
TRACE_DIR="${TRACE_DIR:-$PWD}"
NODE="${RABBITMQ_NODE:-rabbit@$(hostname -s)}"
TRACE_FILE="${TRACE_DIR}/${TESTCASE}.json"

print_cases() {
  cat <<'CASES'
Available test cases:
  1) feature-flag-enable
  2) plugins-set
  3) queue-rebalance
  4) management-sequence
  5) plugin-toggle
  6) rebalance-loop
CASES
}

if [[ "$TESTCASE" == "list" ]]; then
  print_cases
  exit 0
fi

mkdir -p "$TRACE_DIR"
rm -f "$TRACE_FILE"

export RABBITMQ_APP_TRACE_ENABLED=true
export RABBITMQ_APP_TRACE_DIR="$TRACE_DIR"
export RABBITMQ_APP_TRACE_TESTCASE="$TESTCASE"

run_case() {
  case "$TESTCASE" in
    feature-flag-enable)
      ./sbin/rabbitmqctl -n "$NODE" enable_feature_flag all
      ;;

    plugins-set)
      ./sbin/rabbitmq-plugins -n "$NODE" set rabbitmq_management rabbitmq_prometheus
      ;;

    queue-rebalance)
      ./sbin/rabbitmq-queues -n "$NODE" rebalance all
      ;;

    management-sequence)
      ./sbin/rabbitmqctl -n "$NODE" enable_feature_flag all
      ./sbin/rabbitmq-plugins -n "$NODE" set rabbitmq_management rabbitmq_prometheus
      ./sbin/rabbitmq-queues -n "$NODE" rebalance all
      ;;

    plugin-toggle)
      ./sbin/rabbitmq-plugins -n "$NODE" set rabbitmq_management
      ./sbin/rabbitmq-plugins -n "$NODE" set rabbitmq_management rabbitmq_prometheus
      ;;

    rebalance-loop)
      ./sbin/rabbitmq-queues -n "$NODE" rebalance all
      ./sbin/rabbitmq-queues -n "$NODE" rebalance all
      ./sbin/rabbitmq-queues -n "$NODE" rebalance all
      ;;

    *)
      echo "Unknown test case: $TESTCASE"
      print_cases
      exit 2
      ;;
  esac
}

run_case

if [[ ! -f "$TRACE_FILE" ]]; then
  echo "Trace file was not generated: $TRACE_FILE"
  exit 3
fi

echo "Trace generated: $TRACE_FILE"
