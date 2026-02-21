## This Source Code Form is subject to the terms of the Mozilla Public
## License, v. 2.0. If a copy of the MPL was not distributed with this
## file, You can obtain one at https://mozilla.org/MPL/2.0/.
##
## Copyright (c) 2007-2026 Broadcom. All Rights Reserved. The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.  All rights reserved.

defmodule RabbitMQ.CLI.Core.TraceLogger do
  @moduledoc false

  @enabled_env "RABBITMQ_APP_TRACE_ENABLED"
  @trace_dir_env "RABBITMQ_APP_TRACE_DIR"
  @test_case_env "RABBITMQ_APP_TRACE_TESTCASE"

  def emit(event_type, before_state, after_state, result, details \\ %{}) do
    if enabled?() do
      write_event(event_type, before_state, after_state, result, details)
    end

    :ok
  rescue
    _ ->
      :ok
  end

  defp enabled?() do
    case System.get_env(@enabled_env, "false") |> String.downcase() do
      "1" -> true
      "true" -> true
      "yes" -> true
      "on" -> true
      _ -> false
    end
  end

  defp write_event(event_type, before_state, after_state, result, details) do
    test_case = trace_test_case()
    path = trace_file_path(test_case)
    doc = load_doc(path, test_case)
    events = Map.get(doc, "events", [])

    event =
      %{
        "stepSeq" => length(events) + 1,
        "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601(),
        "eventType" => to_string(event_type),
        "before" => normalize(before_state),
        "after" => normalize(after_state),
        "result" => normalize(result),
        "details" => normalize(details)
      }

    new_doc =
      doc
      |> Map.put("schemaVersion", 1)
      |> Map.put("testCase", test_case)
      |> Map.put("events", events ++ [event])

    {:ok, encoded} = JSON.encode(new_doc)
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, encoded <> "\n")
  end

  defp trace_test_case() do
    System.get_env(@test_case_env, "manual")
    |> String.trim()
    |> case do
      "" -> "manual"
      value -> value
    end
    |> String.replace(~r/[^A-Za-z0-9_.-]/, "_")
  end

  defp trace_file_path(test_case) do
    trace_dir = System.get_env(@trace_dir_env, File.cwd!())
    Path.join(trace_dir, "#{test_case}.json")
  end

  defp load_doc(path, test_case) do
    case File.read(path) do
      {:ok, body} ->
        case String.trim(body) do
          "" ->
            base_doc(test_case)

          non_empty ->
            case JSON.decode(non_empty) do
              {:ok, %{} = decoded} -> decoded
              _ -> base_doc(test_case)
            end
        end

      _ ->
        base_doc(test_case)
    end
  end

  defp base_doc(test_case) do
    %{
      "schemaVersion" => 1,
      "testCase" => test_case,
      "events" => []
    }
  end

  defp normalize(value) when is_binary(value) or is_boolean(value) or is_nil(value) do
    value
  end

  defp normalize(value) when is_integer(value) or is_float(value) do
    value
  end

  defp normalize(value) when is_atom(value) do
    Atom.to_string(value)
  end

  defp normalize(value) when is_map(value) do
    value
    |> Enum.map(fn {k, v} -> {to_string(k), normalize(v)} end)
    |> Map.new()
  end

  defp normalize(value) when is_list(value) do
    Enum.map(value, &normalize/1)
  end

  defp normalize(value) when is_tuple(value) do
    value
    |> Tuple.to_list()
    |> Enum.map(&normalize/1)
  end

  defp normalize(value) do
    inspect(value)
  end
end
