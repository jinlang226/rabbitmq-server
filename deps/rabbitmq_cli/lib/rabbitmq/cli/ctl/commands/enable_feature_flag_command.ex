## This Source Code Form is subject to the terms of the Mozilla Public
## License, v. 2.0. If a copy of the MPL was not distributed with this
## file, You can obtain one at https://mozilla.org/MPL/2.0/.
##
## Copyright (c) 2007-2026 Broadcom. All Rights Reserved. The term “Broadcom” refers to Broadcom Inc. and/or its subsidiaries. All rights reserved.

defmodule RabbitMQ.CLI.Ctl.Commands.EnableFeatureFlagCommand do
  @behaviour RabbitMQ.CLI.CommandBehaviour

  alias RabbitMQ.CLI.Core.TraceLogger

  def switches(), do: [experimental: :boolean, opt_in: :boolean]
  def aliases(), do: [e: :experimental, o: :opt_in]

  def merge_defaults(args, opts),
    do: {args, Map.merge(%{experimental: false, opt_in: false}, opts)}

  def validate([], _opts), do: {:validation_failure, :not_enough_args}

  def validate([_ | _] = args, _opts) when length(args) > 1,
    do: {:validation_failure, :too_many_args}

  def validate([""], _opts),
    do:
      {:validation_failure,
       {:bad_argument, "feature flag (or group) name cannot be an empty string"}}

  def validate([_], _opts), do: :ok

  use RabbitMQ.CLI.Core.RequiresRabbitAppRunning

  def run(["all"], %{node: node_name, opt_in: opt_in, experimental: experimental}) do
    has_opted_in = opt_in || experimental
    enable_all(node_name, has_opted_in)
  end

  def run(["all"], %{node: node_name, opt_in: has_opted_in}) do
    enable_all(node_name, has_opted_in)
  end

  def run(["all"], %{node: node_name, experimental: has_opted_in}) do
    enable_all(node_name, has_opted_in)
  end

  def run(["all"], %{node: node_name}) do
    enable_all(node_name, false)
  end

  def run([feature_flag], %{node: node_name, opt_in: opt_in, experimental: experimental}) do
    has_opted_in = opt_in || experimental
    enable_one(node_name, feature_flag, has_opted_in)
  end

  def run([feature_flag], %{node: node_name, opt_in: has_opted_in}) do
    enable_one(node_name, feature_flag, has_opted_in)
  end

  def run([feature_flag], %{node: node_name, experimental: has_opted_in}) do
    enable_one(node_name, feature_flag, has_opted_in)
  end

  def run([feature_flag], %{node: node_name}) do
    enable_one(node_name, feature_flag, false)
  end

  def output({:error, :unsupported}, %{node: node_name}) do
    {:error, RabbitMQ.CLI.Core.ExitCodes.exit_usage(),
     "This feature flag is not supported by node #{node_name}"}
  end

  use RabbitMQ.CLI.DefaultOutput

  def usage, do: "enable_feature_flag [--opt-in] <all | feature_flag>"

  def usage_additional() do
    [
      [
        "<feature_flag>",
        "name of the feature flag to enable, or \"all\" to enable all supported flags"
      ],
      [
        "--opt-in",
        "required to enable certain feature flags (those with vast scope or maturing)"
      ]
    ]
  end

  def help_section(), do: :feature_flags

  def description(),
    do: "Enables a feature flag or all supported feature flags on the target node"

  def banner(["all"], _), do: "Enabling all feature flags ..."

  def banner([feature_flag], _), do: "Enabling feature flag \"#{feature_flag}\" ..."

  #
  # Implementation
  #

  defp enable_all(node_name, has_opted_in) do
    before = feature_flags_snapshot(node_name)

    result =
      case has_opted_in do
        true ->
          msg =
            "`--opt-in` (aliased as `--experimental`) flag is not allowed when enabling all feature flags.\nUse --opt-in with a specific feature flag name if to enable an opt-in flag"

          {:error, RabbitMQ.CLI.Core.ExitCodes.exit_usage(), msg}

        _ ->
          case :rabbit_misc.rpc_call(node_name, :rabbit_feature_flags, :enable_all, []) do
            {:badrpc, _} = err -> err
            other -> other
          end
      end

    trace_enable_result(node_name, "all", has_opted_in, result, before)
  end

  defp enable_one(node_name, feature_flag, has_opted_in) do
    before = feature_flags_snapshot(node_name)

    result =
      case {has_opted_in,
            :rabbit_misc.rpc_call(node_name, :rabbit_feature_flags, :get_stability, [
              String.to_atom(feature_flag)
            ])} do
        {_, {:badrpc, _} = err} ->
          err

        {false, :experimental} ->
          msg =
            "Feature flag #{feature_flag} requires the user to explicitly opt-in.\nUse --opt-in with a specific feature flag name if to enable an opt-in flag"

          {:error, RabbitMQ.CLI.Core.ExitCodes.exit_usage(), msg}

        _ ->
          case :rabbit_misc.rpc_call(node_name, :rabbit_feature_flags, :enable, [
                 String.to_atom(feature_flag)
               ]) do
            {:badrpc, _} = err -> err
            other -> other
          end
      end

    trace_enable_result(node_name, feature_flag, has_opted_in, result, before)
  end

  defp trace_enable_result(node_name, feature_flag, has_opted_in, result, before) do
    success = command_success?(result)
    reason = reason_for_result(result)
    after_state = feature_flags_snapshot(node_name)

    TraceLogger.emit(
      event_type_for_enable(success),
      before,
      after_state,
      %{"success" => success, "raw" => inspect(result)},
      %{
        "node" => to_string(node_name),
        "featureFlag" => feature_flag,
        "optIn" => has_opted_in,
        "reason" => reason
      }
    )

    result
  end

  defp event_type_for_enable(true), do: "FeatureFlagsEnabled"
  defp event_type_for_enable(false), do: "EnableFeatureFlagsFailed"

  defp reason_for_result({:error, _, msg}), do: inspect(msg)
  defp reason_for_result({:error, msg}), do: inspect(msg)
  defp reason_for_result({:badrpc, reason}), do: inspect(reason)
  defp reason_for_result(_), do: "ok"

  defp feature_flags_snapshot(node_name) do
    case :rabbit_misc.rpc_call(node_name, :rabbit_feature_flags, :list, [:enabled]) do
      {:badrpc, _} = err ->
        %{"rpcError" => inspect(err)}

      %{} = flags ->
        enabled_flags =
          flags
          |> Map.keys()
          |> Enum.map(&to_string/1)
          |> Enum.sort()

        %{
          "enabledFeatureFlags" => enabled_flags,
          "enabledFeatureFlagCount" => length(enabled_flags)
        }

      other ->
        %{"raw" => inspect(other)}
    end
  end

  defp command_success?({:error, _, _}), do: false
  defp command_success?({:error, _}), do: false
  defp command_success?({:badrpc, _}), do: false
  defp command_success?(_), do: true
end
