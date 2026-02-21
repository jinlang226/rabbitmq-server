# RabbitMQ Application Trace Logging (Model-Conformance)

This guide explains how to run RabbitMQ in Ubuntu, execute application-level trace test cases, and produce per-test-case JSON trace files.

## What was added

The following CLI command paths now emit structured trace events when enabled:

- `enable_feature_flag` (`EnableFeatureFlagsCommand`)
- `rabbitmq-plugins set ...` (`SetPluginsCommand`)
- `rabbitmq-queues rebalance ...` (`QueueRebalanceCommand`)

Trace events include:

- `eventType`
- `stepSeq`
- `timestamp`
- `before`
- `after`
- `result`
- `details`

A trace file is written as:

- `<TRACE_DIR>/<TESTCASE>.json`

By default, `TRACE_DIR` is the current directory where you run the test script.

---

## 1) Ubuntu setup

### 1.1 Install dependencies

```bash
sudo apt-get update
sudo apt-get install -y \
  build-essential \
  git \
  make \
  curl \
  erlang \
  elixir \
  python3
```

### 1.2 Build and run RabbitMQ locally

From repository root:

```bash
gmake virgin-test-tmpdir ENABLED_PLUGINS="rabbitmq_management rabbitmq_prometheus" run-broker
```

If `gmake` is not available on your VM, use `make`.

Keep this terminal running.

---

## 2) Run application trace test cases

Open a second terminal in the same repository root.

### 2.1 List available test cases

```bash
./scripts/run_app_trace_case.sh list
```

### 2.2 Run one test case

```bash
./scripts/run_app_trace_case.sh feature-flag-enable
```

### 2.3 Run all test cases

```bash
./scripts/run_app_trace_suite.sh
```

---

## 3) How many test cases are provided

There are **6** test cases:

1. `feature-flag-enable`
2. `plugins-set`
3. `queue-rebalance`
4. `management-sequence`
5. `plugin-toggle`
6. `rebalance-loop`

Each test case generates exactly one trace file named `<testcase>.json`.

Examples:

- `feature-flag-enable.json`
- `management-sequence.json`

---

## 4) Trace output location and naming

By default, trace files are created in your current working directory.

You can override output directory and node:

```bash
export TRACE_DIR="$PWD/traces"
export RABBITMQ_NODE="rabbit@$(hostname -s)"
./scripts/run_app_trace_case.sh management-sequence
```

Output file example:

- `traces/management-sequence.json`

---

## 5) Useful operational commands

### Stop broker

```bash
gmake stop-node
```

### Start broker again (clean tmpdir)

```bash
gmake virgin-test-tmpdir ENABLED_PLUGINS="rabbitmq_management rabbitmq_prometheus" run-broker
```

---

## 6) Notes for model conformance use

- Trace logging is enabled only when `RABBITMQ_APP_TRACE_ENABLED=true`.
- The testcase name comes from `RABBITMQ_APP_TRACE_TESTCASE` and is sanitized for file naming.
- `before/after` snapshots are captured at command boundaries in CLI command handlers.

If you need additional event families (for example publish/consume data-plane traces), add instrumentation in the corresponding command/API path and keep the same JSON schema.
