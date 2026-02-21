## This Source Code Form is subject to the terms of the Mozilla Public
## License, v. 2.0. If a copy of the MPL was not distributed with this
## file, You can obtain one at https://mozilla.org/MPL/2.0/.
##
## Copyright (c) 2007-2026 Broadcom. All Rights Reserved. The term “Broadcom” refers to Broadcom Inc. and/or its subsidiaries.  All rights reserved.

defmodule RabbitMQ.CLI.Upgrade.Commands.AwaitOnlineQuorumPlusOneCommand do
  alias RabbitMQ.CLI.Core.DocGuide
  alias RabbitMQ.CLI.Core.TraceLogger
  import RabbitMQ.CLI.Core.Config, only: [output_less?: 1]

  @behaviour RabbitMQ.CLI.CommandBehaviour

  @default_timeout 120_000

  use RabbitMQ.CLI.Core.RequiresRabbitAppRunning
  use RabbitMQ.CLI.Core.AcceptsNoPositionalArguments

  def merge_defaults(args, opts) do
    timeout =
      case opts[:timeout] do
        nil -> @default_timeout
        :infinity -> @default_timeout
        val -> val
      end

    {args, Map.put(opts, :timeout, timeout)}
  end

  def run([], %{node: node_name, timeout: timeout}) do
    before = quorum_snapshot(node_name)
    rpc_timeout = timeout + 500

    result =
      case :rabbit_misc.rpc_call(node_name, :rabbit_nodes, :is_single_node_cluster, [], rpc_timeout) do
        # if target node is the only one in the cluster, the command makes little sense
        # and false positives can be misleading
        true ->
          {:ok, :single_node_cluster}

        false ->
          case :rabbit_misc.rpc_call(
                 node_name,
                 :rabbit_upgrade_preparation,
                 :await_online_quorum_plus_one,
                 [timeout],
                 rpc_timeout
               ) do
            {:error, _} = err ->
              err

            {:error, _, _} = err ->
              err

            {:badrpc, _} = err ->
              err

            true ->
              :ok

            false ->
              {:error,
               "time is up, no quorum + 1 online replicas came online for at least some quorum queues or streams"}
          end

        other ->
          other
      end

    success = command_success?(result)
    TraceLogger.emit(
      event_type_for_await(success),
      before,
      quorum_snapshot(node_name),
      %{"success" => success, "raw" => inspect(result)},
      %{
        "node" => to_string(node_name),
        "timeoutMs" => timeout,
        "reason" => reason_for_result(result)
      }
    )

    result
  end

  def output({:ok, :single_node_cluster}, %{formatter: "json"}) do
    {:ok,
     %{
       "result" => "ok",
       "message" =>
         "Target node seems to be the only one in a single node cluster, the check does not apply"
     }}
  end

  def output({:error, msg}, %{node: node_name, formatter: "json"}) do
    {:error, %{"result" => "error", "node" => node_name, "message" => msg}}
  end

  def output({:ok, :single_node_cluster}, opts) do
    case output_less?(opts) do
      true ->
        :ok

      false ->
        {:ok,
         "Target node seems to be the only one in a single node cluster, the command does not apply"}
    end
  end

  use RabbitMQ.CLI.DefaultOutput

  def usage, do: "await_online_quorum_plus_one"

  def usage_doc_guides() do
    [
      DocGuide.quorum_queues(),
      DocGuide.upgrade()
    ]
  end

  def help_section, do: :upgrade

  def description() do
    "Waits for all quorum queues and streams to have an above minimum online quorum. " <>
      "This makes sure that no queues/streams would lose their quorum if the target node is shut down"
  end

  def banner([], %{timeout: timeout}) do
    "Will wait for a quorum + 1 of nodes to be online for all quorum queues and streams for #{round(timeout / 1000)} seconds..."
  end

  defp quorum_snapshot(node_name) do
    single_node =
      case :rabbit_misc.rpc_call(node_name, :rabbit_nodes, :is_single_node_cluster, []) do
        true -> true
        false -> false
        _ -> nil
      end

    running_nodes =
      case :rabbit_misc.rpc_call(node_name, :rabbit_nodes, :list_running, []) do
        nodes when is_list(nodes) ->
          nodes
          |> Enum.map(&to_string/1)
          |> Enum.sort()

        _ ->
          []
      end

    %{
      "singleNodeCluster" => single_node,
      "runningNodeCount" => length(running_nodes),
      "runningNodes" => running_nodes
    }
  end

  defp command_success?(:ok), do: true
  defp command_success?({:ok, :single_node_cluster}), do: true
  defp command_success?(_), do: false

  defp event_type_for_await(true), do: "AwaitOnlineQuorumPlusOne"
  defp event_type_for_await(false), do: "AwaitOnlineQuorumPlusOneFailed"

  defp reason_for_result(:ok), do: "ok"
  defp reason_for_result({:ok, :single_node_cluster}), do: "single_node_cluster"
  defp reason_for_result({:error, msg}), do: inspect(msg)
  defp reason_for_result({:error, _, msg}), do: inspect(msg)
  defp reason_for_result({:badrpc, reason}), do: inspect(reason)
  defp reason_for_result(other), do: inspect(other)
end
