# RabbitMQ Application Trace Logging (Model-Conformance)

This guide explains how to run RabbitMQ in Ubuntu, execute application-level trace test cases, and produce per-test-case JSON trace files.

## What was added

The following command and API paths now emit structured trace events when enabled:

- `enable_feature_flag` (`FeatureFlagsEnabled` / `EnableFeatureFlagsFailed`)
- `rabbitmq-plugins set ...` (`SetPlugins` / `SetPluginsFailed`)
- `rabbitmq-queues rebalance ...` (`QueueRebalance` / `QueueRebalanceFailed`)
- `rabbitmq-upgrade await_online_quorum_plus_one` (`AwaitOnlineQuorumPlusOne` / `AwaitOnlineQuorumPlusOneFailed`)
- Management API publish (`PublishMessage` / `PublishMessageFailed`)
- Management API queue get (`ConsumeMessage`)

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

By default, `TRACE_DIR` is `<repo-root>/traces`.

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
  python3
```

### 1.2 Install Erlang/OTP 27.3.4.2 (required)

RabbitMQ in this repository targets Erlang 27.x. On Ubuntu 22.04, install OTP 27.3.4.2 with `kerl`:

```bash
sudo apt-get update
sudo apt-get install -y \
  autoconf m4 build-essential \
  libssl-dev libncurses-dev libwxgtk3.0-gtk3-dev \
  libgl1-mesa-dev libglu1-mesa-dev libpng-dev libssh-dev \
  unixodbc-dev xsltproc fop libxml2-utils

git clone https://github.com/kerl/kerl.git ~/.kerl
export PATH="$HOME/.kerl:$PATH"
chmod +x "$HOME/.kerl/kerl"

export KERL_CONFIGURE_OPTIONS="--without-javac --without-wx"

$HOME/.kerl/kerl update releases
$HOME/.kerl/kerl build 27.3.4.2 27.3.4.2
$HOME/.kerl/kerl install 27.3.4.2 $HOME/.kerl/installs/27.3.4.2
source $HOME/.kerl/installs/27.3.4.2/activate

erl -noshell -eval 'io:format("OTP=~s~n",[erlang:system_info(otp_release)]),halt().'
```

Expected output should include `OTP=27`.

### 1.3 Build and run RabbitMQ locally

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

There are **8** test cases:

1. `feature-flag-enable`
2. `plugins-set`
3. `queue-rebalance`
4. `await-online-quorum-plus-one`
5. `publish-consume`
6. `management-sequence`
7. `plugin-toggle`
8. `rebalance-loop`

Each test case generates exactly one trace file named `<testcase>.json`.

Examples:

- `feature-flag-enable.json`
- `management-sequence.json`

---

## 4) Trace output location and naming

By default, trace files are created in `<repo-root>/traces`.

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
- `before/after` snapshots are captured at command boundaries in CLI and management handlers.
- Event names are model-aligned success/failure pairs (for example `SetPlugins` vs `SetPluginsFailed`).
- Management API traces can also be enabled per request with:
  - `x-rabbitmq-trace-enabled: true`
  - `x-rabbitmq-trace-testcase: <name>`

If you need additional event families (for example publish/consume data-plane traces), add instrumentation in the corresponding command/API path and keep the same JSON schema.
