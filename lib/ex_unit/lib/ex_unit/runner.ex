defmodule ExUnit.Runner do
  @moduledoc false

  alias ExUnit.EventManager, as: EM
  alias ExUnit.RunnerStats

  @rand_algorithm :exs1024

  @type manager :: ExUnit.EventManager.manager()

  def run(opts, load_us) do
    {:ok, manager} = EM.start_link()
    {:ok, stat_sup} = EM.add_handler(manager, RunnerStats, opts)
    {opts, config} = configure(manager, opts)
    config = Map.put(config, :stat_sup, stat_sup)
    config = Map.put(config, :top_pid, self())
    :erlang.system_flag(:backtrace_depth, Keyword.fetch!(opts, :stacktrace_depth))

    {run_us, _} =
      :timer.tc(fn ->
        EM.suite_started(config.manager, opts)
        loop(config, 0)
      end)

    if max_failures_reached?(config.stat_sup, config.max_failures) do
      EM.notify(config.manager, :aborting_max_failures_reached)
    end

    EM.suite_finished(config.manager, run_us, load_us)
    stats = RunnerStats.stats(stat_sup)

    EM.stop(config.manager)

    after_suite_callbacks = Application.fetch_env!(:ex_unit, :after_suite)
    Enum.each(after_suite_callbacks, fn callback -> callback.(stats) end)
    stats
  end

  defp configure(manager, opts) do
    opts = normalize_opts(opts)
    Enum.each(opts[:formatters], &EM.add_handler(manager, &1, opts))

    config = %{
      capture_log: opts[:capture_log],
      exclude: opts[:exclude],
      include: opts[:include],
      manager: manager,
      max_cases: opts[:max_cases],
      max_failures: opts[:max_failures],
      only_test_ids: opts[:only_test_ids],
      seed: opts[:seed],
      modules: :async,
      timeout: opts[:timeout],
      trace: opts[:trace]
    }

    {opts, config}
  end

  defp normalize_opts(opts) do
    {include, exclude} = ExUnit.Filters.normalize(opts[:include], opts[:exclude])

    opts
    |> Keyword.put(:exclude, exclude)
    |> Keyword.put(:include, include)
  end

  defp loop(%{modules: :async} = config, taken) do
    available = config.max_cases - taken

    cond do
      # No modules available, wait for one
      available <= 0 ->
        wait_until_available(config, taken)

      # Slots are available, start with async modules
      modules = ExUnit.Server.take_async_modules(available) ->
        spawn_modules(modules, taken, config)

      true ->
        modules = ExUnit.Server.take_sync_modules()
        loop(%{config | modules: modules}, taken)
    end
  end

  defp loop(%{modules: modules} = config, taken) do
    case modules do
      _ when taken > 0 ->
        wait_until_available(config, taken)

      # So we can start all sync modules
      [head | tail] ->
        spawn_modules([head], taken, %{config | modules: tail})

      # No more modules, we are done!
      [] ->
        :ok
    end
  end

  # Loop expecting messages from the spawned modules. Whenever
  # a module has finished executing, decrease the taken modules
  # counter and attempt to spawn new ones.
  defp wait_until_available(config, taken) do
    receive do
      {_pid, :module_finished} ->
        loop(config, taken - 1)

      # end loop
      {_pid, :max_failures_reached} ->
        {:error, :max_failures_reached}
    end
  end

  defp spawn_modules(modules, taken, config) do
    Enum.each(modules, fn module ->
      spawn_link(fn ->
        run_module(module, config)
      end)
    end)

    loop(config, taken + length(modules))
  end

  defp run_module(module, config) do
    if max_failures_reached?(config.stat_sup, config.max_failures) do
      send(config.top_pid, {self(), :max_failures_reached})
    else
      test_module = module.__ex_unit__()
      EM.module_started(config.manager, test_module)

      # Prepare tests, selecting which ones should be run or skipped
      tests = prepare_tests(test_module.tests, config)

      {test_module, test_results} =
        if Enum.all?(tests, & &1.state) do
          # The pending tests here aren't actually run,
          # so they're marked as "finished".
          {test_module, %{pending: tests, finished: tests}}
        else
          spawn_module(test_module, tests, config)
        end

      # Do not run pending tests.
      # Just send the notifications to the formatter.
      Enum.each(test_results.pending, fn test ->
        EM.test_started(config.manager, test)
        EM.test_finished(config.manager, test)
      end)

      test_module = %{test_module | tests: test_results.finished}
      EM.module_finished(config.manager, test_module)

      # message loop/2
      send(config.top_pid, {self(), :module_finished})
    end
  end

  defp prepare_tests(tests, config) do
    tests = shuffle(tests, config)
    include = config.include
    exclude = config.exclude
    test_ids = config.only_test_ids

    for test <- tests, include_test?(test_ids, test) do
      tags = Map.merge(test.tags, %{test: test.name, module: test.module})

      case ExUnit.Filters.eval(include, exclude, tags, tests) do
        :ok -> %{test | tags: tags}
        excluded_or_skipped -> %{test | state: excluded_or_skipped}
      end
    end
  end

  defp include_test?(nil, _test), do: true

  defp include_test?(test_ids, test) do
    MapSet.member?(test_ids, {test.module, test.name})
  end

  defp spawn_module(test_module, tests, config) do
    parent_pid = self()
    timeout = get_timeout(%{}, config)

    {module_pid, module_ref} = spawn_module_monitor(test_module, parent_pid, tests, config)
    options = %{pid: module_pid, config: config}

    {test_module, test_results} =
      receive do
        {^module_pid, :module_finished, test_module, tests} ->
          process_failure(test_module, options)
          Process.demonitor(module_ref, [:flush])
          {test_module, tests}

        {:DOWN, ^module_ref, :process, ^module_pid, error} ->
          test_module = %{test_module | state: failed({:EXIT, module_pid}, error, [])}
          process_failure(test_module, options)
          {test_module, %{pending: [], finished: []}}
      end

    test_module = exec_on_exit(test_module, module_pid, timeout)
    {test_module, test_results}
  end

  defp spawn_module_monitor(test_module, parent_pid, tests, config) do
    spawn_monitor(fn ->
      module_pid = self()
      register_test_module(module_pid)

      case exec_module_setup(test_module) do
        {:ok, _test_module, context} ->
          # max_failures can be reached during the execution of test_module,
          # so we keep track of which tests are not executed and which ones are finished.
          tests_run = run_tests(tests, context, config)

          send(
            parent_pid,
            {module_pid, :module_finished, test_module,
             %{pending: [], finished: tests_run.finished}}
          )

        {:error, test_module} ->
          invalid_tests = Enum.map(tests, &%{&1 | state: {:invalid, test_module}})

          send(
            parent_pid,
            {module_pid, :module_finished, test_module, %{pending: invalid_tests, finished: []}}
          )
      end

      exit(:shutdown)
    end)
  end

  @spec run_tests(list, list, map) :: %{
          :not_executed => list(),
          :finished => list()
        }
  defp run_tests(tests, context, config) do
    Enum.map(tests, fn test ->
      run_test(test, context, config)
    end)
    |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
    |> Map.put_new(:not_executed, [])
    |> Map.put_new(:finished, [])
  end

  defp exec_module_setup(%ExUnit.TestModule{name: module} = test_module) do
    {:ok, test_module, module.__ex_unit__(:setup_all, %{module: module, case: module})}
  catch
    kind, error ->
      failed = failed(kind, error, prune_stacktrace(__STACKTRACE__))
      {:error, %{test_module | state: failed}}
  end

  defp run_test_with_capture_log(test, true, context, config) do
    run_test_with_capture_log(test, [], context, config)
  end

  defp run_test_with_capture_log(test, false, context, config) do
    spawn_test(test, context, config)
  end

  defp run_test_with_capture_log(test, opts, context, config) do
    ref = make_ref()

    try do
      ExUnit.CaptureLog.capture_log(opts, fn ->
        send(self(), {ref, spawn_test(test, context, config)})
      end)
    catch
      :exit, :noproc ->
        message =
          "could not run test, it uses @tag :capture_log" <>
            " but the :logger application is not running"

        test = %{test | state: failed(:error, RuntimeError.exception(message), [])}
        {:finished, test}
    else
      logged ->
        receive do
          {^ref, {status, test}} ->
            {status, %{test | logs: logged}}
        end
    end
  end

  defp run_test(%{tags: tags} = test, context, config) do
    if max_failures_reached?(config.stat_sup, config.max_failures) do
      {:not_executed, test}
    else
      EM.test_started(config.manager, test)

      # We need to retrieve the status, because the test can be aborted
      # during its execution due to max_failures being reached
      {status, test} =
        if is_nil(test.state) do
          capture_log = Map.get(tags, :capture_log, config.capture_log)
          run_test_with_capture_log(test, capture_log, Map.merge(tags, context), config)
        else
          {:finished, test}
        end

      case status do
        :finished ->
          EM.test_finished(config.manager, test)

        :not_executed ->
          nil
      end

      {status, test}
    end
  end

  defp spawn_test(test, context, config) do
    parent_pid = self()
    timeout = get_timeout(test.tags, config)

    {test_pid, test_ref} = spawn_test_monitor(test, parent_pid, context, config)

    reply_options = %{
      pid: test_pid,
      ref: test_ref,
      parent_pid: parent_pid,
      timeout: timeout,
      config: config
    }

    {status, test} = receive_test_reply(test, reply_options)
    {status, exec_on_exit(test, test_pid, timeout)}
  end

  defp spawn_test_monitor(test, parent_pid, context, config) do
    spawn_monitor(fn ->
      test_pid = self()
      generate_test_seed(test, config)

      register_test(test_pid, config.stat_sup)

      {time, test} =
        :timer.tc(fn ->
          case exec_test_setup(test, context) do
            {:ok, test} ->
              exec_test(test)

            {:error, test} ->
              test
          end
        end)

      send(parent_pid, {test_pid, :test_finished, %{test | time: time}})
      exit(:shutdown)
    end)
  end

  defp receive_test_reply(
         test,
         %{pid: test_pid, ref: test_ref, timeout: timeout} = options
       ) do
    receive do
      {^test_pid, :test_finished, test} ->
        process_failure(test, options)
        Process.demonitor(test_ref, [:flush])
        {:finished, test}

      {:DOWN, ^test_ref, :process, ^test_pid, :exit_max_failures_reached} ->
        {:not_executed, test}

      {:DOWN, ^test_ref, :process, ^test_pid, error} ->
        test = %{test | state: failed({:EXIT, test_pid}, error, [])}
        process_failure(test, options)
        {:finished, test}
    after
      timeout ->
        case Process.info(test_pid, :current_stacktrace) do
          {:current_stacktrace, stacktrace} ->
            Process.demonitor(test_ref, [:flush])
            Process.exit(test_pid, :kill)

            exception =
              ExUnit.TimeoutError.exception(
                timeout: timeout,
                type: Atom.to_string(test.tags.test_type)
              )

            test = %{test | state: failed(:error, exception, stacktrace)}
            process_failure(test, options)
            {:finished, test}

          nil ->
            receive_test_reply(test, options)
        end
    end
  end

  defp exec_test_setup(%ExUnit.Test{module: module} = test, context) do
    {:ok, %{test | tags: module.__ex_unit__(:setup, context)}}
  catch
    kind, error ->
      {:error, %{test | state: failed(kind, error, prune_stacktrace(__STACKTRACE__))}}
  end

  defp exec_test(%ExUnit.Test{module: module, name: name, tags: context} = test) do
    apply(module, name, [context])
    test
  catch
    kind, error ->
      %{test | state: failed(kind, error, prune_stacktrace(__STACKTRACE__))}
  end

  defp exec_on_exit(test_or_case, pid, timeout) do
    if ExUnit.OnExitHandler.registered?(pid) do
      case ExUnit.OnExitHandler.run(pid, timeout) do
        :ok ->
          test_or_case

        {kind, reason, stack} ->
          state = test_or_case.state || failed(kind, reason, prune_stacktrace(stack))
          %{test_or_case | state: state}
      end
    else
      test_or_case
    end
  end

  ## Helpers

  defp generate_test_seed(%ExUnit.Test{module: module, name: name}, %{seed: seed}) do
    :rand.seed(@rand_algorithm, {:erlang.phash2(module), :erlang.phash2(name), seed})
  end

  defp register_test(pid, stat_sup) do
    ExUnit.OnExitHandler.register(pid)
    RunnerStats.register(stat_sup, pid)
  end

  defp register_test_module(pid) do
    ExUnit.OnExitHandler.register(pid)
  end

  defp get_failure_counter(stat_sup) when is_pid(stat_sup),
    do: RunnerStats.get_failure_counter(stat_sup)

  defp increment_failure_counter(stat_sup, %struct{state: {tag, _}}, increment \\ 1)
       when struct in [ExUnit.Test, ExUnit.TestModule] and tag in [:failed, :invalid],
       do: RunnerStats.increment_failure_counter(stat_sup, increment)

  # Takes care of the logic when the failure counter should be incremented,
  # as well as stopping the suite if max_failures has been reached
  defp process_failure(
         %ExUnit.TestModule{state: {tag, _}, tests: tests} = test_module,
         %{config: config} = _options
       )
       when tag in [:failed, :invalid] do
    failure_counter = increment_failure_counter(config.stat_sup, test_module, length(tests))

    if max_failures_reached?(failure_counter, config.max_failures) do
      max_failures_has_been_reached(config.manager, config.stat_sup, nil)
      {:error, :max_failures_reached}
    else
      :ok
    end
  end

  defp process_failure(%ExUnit.TestModule{} = _test_module, _options) do
    :ok
  end

  defp process_failure(
         %ExUnit.Test{state: {:failed, _}} = test,
         %{pid: test_pid, config: config} = _options
       ) do
    failure_counter = increment_failure_counter(config.stat_sup, test)

    if max_failures_reached?(failure_counter, config.max_failures) do
      max_failures_has_been_reached(config.manager, config.stat_sup, test_pid)
      {:error, :max_failures_reached}
    else
      :ok
    end
  end

  defp process_failure(%ExUnit.Test{} = _test, _options) do
    :ok
  end

  # Notifies the event manager that max_failures has been reached
  # and stops all running tests in the suite, except test_pid
  defp max_failures_has_been_reached(manager, stat_sup, test_pid)
       when is_tuple(manager) and is_pid(stat_sup) and (is_pid(test_pid) or is_nil(test_pid)) do
    EM.max_failures_reached(manager)

    # Note that we never kill any test_module. Actually they are not even registered in RunnerStats.
    for registered_pid <- RunnerStats.get_registered_pids(stat_sup),
        registered_pid != test_pid,
        Process.alive?(registered_pid) do
      Process.exit(registered_pid, :exit_max_failures_reached)
    end
  end

  defp max_failures_reached?(_stat_sup_or_failure_counter, :infinity),
    do: false

  defp max_failures_reached?(stat_sup, max_failures)
       when is_pid(stat_sup) and is_integer(max_failures) do
    get_failure_counter(stat_sup) >= max_failures
  end

  defp max_failures_reached?(failure_counter, max_failures)
       when is_integer(failure_counter) and failure_counter >= 0 and is_integer(max_failures) do
    failure_counter >= max_failures
  end

  defp get_timeout(tags, config) do
    if config.trace() do
      :infinity
    else
      Map.get(tags, :timeout, config.timeout)
    end
  end

  defp shuffle(list, %{seed: 0}) do
    Enum.reverse(list)
  end

  defp shuffle(list, %{seed: seed}) do
    _ = :rand.seed(@rand_algorithm, {seed, seed, seed})
    Enum.shuffle(list)
  end

  defp failed(:error, %ExUnit.MultiError{errors: errors}, _stack) do
    errors =
      Enum.map(errors, fn {kind, reason, stack} ->
        {kind, Exception.normalize(kind, reason, stack), prune_stacktrace(stack)}
      end)

    {:failed, errors}
  end

  defp failed(kind, reason, stack) do
    {:failed, [{kind, Exception.normalize(kind, reason, stack), stack}]}
  end

  # Assertions can pop-up in the middle of the stack
  defp prune_stacktrace([{ExUnit.Assertions, _, _, _} | t]), do: prune_stacktrace(t)

  # As soon as we see a Runner, it is time to ignore the stacktrace
  defp prune_stacktrace([{ExUnit.Runner, _, _, _} | _]), do: []

  # All other cases
  defp prune_stacktrace([h | t]), do: [h | prune_stacktrace(t)]
  defp prune_stacktrace([]), do: []
end
