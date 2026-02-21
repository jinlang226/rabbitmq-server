#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <testcase>"
  echo "Try: $0 list"
  exit 1
fi

TESTCASE="$1"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
TRACE_DIR="${TRACE_DIR:-${REPO_ROOT}/traces}"
NODE="${RABBITMQ_NODE:-rabbit@$(hostname -s)}"
TRACE_FILE="${TRACE_DIR}/${TESTCASE}.json"
MGMT_API="${RABBITMQ_MGMT_API:-http://127.0.0.1:15672/api}"
MGMT_USER="${RABBITMQ_MGMT_USER:-guest}"
MGMT_PASS="${RABBITMQ_MGMT_PASS:-guest}"
TRACE_VHOST_ENCODED="${RABBITMQ_TRACE_VHOST_ENCODED:-%2F}"

print_cases() {
  cat <<'CASES'
Available test cases:
  1) feature-flag-enable
  2) plugins-set
  3) queue-rebalance
  4) await-online-quorum-plus-one
  5) publish-consume
  6) management-sequence
  7) plugin-toggle
  8) rebalance-loop
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

mgmt_call() {
  local method="$1"
  local path="$2"
  local body="${3:-}"
  local url="${MGMT_API}${path}"
  local tmp_file
  local status

  tmp_file="$(mktemp)"

  if [[ -n "$body" ]]; then
    status="$(curl -sS -u "${MGMT_USER}:${MGMT_PASS}" \
      -H "content-type: application/json" \
      -H "x-rabbitmq-trace-enabled: true" \
      -H "x-rabbitmq-trace-testcase: ${TESTCASE}" \
      -X "${method}" "${url}" \
      --data "$body" \
      -o "${tmp_file}" \
      -w "%{http_code}")"
  else
    status="$(curl -sS -u "${MGMT_USER}:${MGMT_PASS}" \
      -H "x-rabbitmq-trace-enabled: true" \
      -H "x-rabbitmq-trace-testcase: ${TESTCASE}" \
      -X "${method}" "${url}" \
      -o "${tmp_file}" \
      -w "%{http_code}")"
  fi

  if [[ "$status" -lt 200 || "$status" -ge 300 ]]; then
    echo "Management API call failed: ${method} ${url} (HTTP ${status})" >&2
    cat "${tmp_file}" >&2
    rm -f "${tmp_file}"
    return 1
  fi

  cat "${tmp_file}"
  rm -f "${tmp_file}"
}

run_publish_consume() {
  local queue
  local payload

  queue="${RABBITMQ_TRACE_QUEUE:-trace-${TESTCASE}-queue}"
  payload="${RABBITMQ_TRACE_PAYLOAD:-trace-${TESTCASE}-payload}"

  mgmt_call PUT "/queues/${TRACE_VHOST_ENCODED}/${queue}" \
    '{"auto_delete":true,"durable":false,"arguments":{}}' >/dev/null

  mgmt_call POST "/exchanges/${TRACE_VHOST_ENCODED}/amq.default/publish" \
    "{\"properties\":{},\"routing_key\":\"${queue}\",\"payload\":\"${payload}\",\"payload_encoding\":\"string\"}" >/dev/null

  mgmt_call POST "/queues/${TRACE_VHOST_ENCODED}/${queue}/get" \
    '{"count":1,"ackmode":"ack_requeue_false","encoding":"auto","truncate":50000}' >/dev/null
}

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

    await-online-quorum-plus-one)
      ./sbin/rabbitmq-upgrade -n "$NODE" await_online_quorum_plus_one
      ;;

    publish-consume)
      run_publish_consume
      ;;

    management-sequence)
      ./sbin/rabbitmqctl -n "$NODE" enable_feature_flag all
      ./sbin/rabbitmq-plugins -n "$NODE" set rabbitmq_management rabbitmq_prometheus
      ./sbin/rabbitmq-queues -n "$NODE" rebalance all
      ./sbin/rabbitmq-upgrade -n "$NODE" await_online_quorum_plus_one
      run_publish_consume
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
