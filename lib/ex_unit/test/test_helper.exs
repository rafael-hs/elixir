Logger.configure_backend(:console, colors: [enabled: false])
ExUnit.start [trace: "--trace" in System.argv]

# Beam files compiled on demand
path = Path.expand("../tmp/beams", __DIR__)
File.rm_rf!(path)
File.mkdir_p!(path)
Code.prepend_path(path)

defmodule ExUnit.TestHelpers do
  require ExUnit.DocTest

  def write_beam({:module, name, bin, _} = res) do
    beam_path = Path.join(unquote(path), Atom.to_string(name) <> ".beam")
    File.write!(beam_path, bin)
    res
  end
end

defmodule ExUnit.TestHelpers.DocTestIsolated do
  @moduledoc """
  These functions and macros to isolate doctests since calling more than once
  on the same module will have issues with functions being already imported if
  :import or :import_iex_helpers options are used.

  `isolate/2` takes `options` as first argument. It can be [async: true | false].

  ## Example

      defmodule ExUnit.DocTestTest do
        use ExUnit.Case
        require ExUnit.TestHelpers.DocTestIsolated, as: DocTestIsolated

        # regular doctest call
        doctest Integer

        # isolated doctest calls
        DocTestIsolated.isolate(async: true) do
          doctest(Kernel, import: true)
          doctest(Kernel, import: [])
        end

        # regular doctest call
        doctest Enum
      end

  """

  defmacro isolate(options \\ [], do: block) do
    quote do
      @__doctest_isolate_async__ unquote(options[:async] || false)
      # TODO : remove this and check if ExUnit.Case have been included using `use`
      Code.ensure_loaded(ExUnit.Case)

      import ExUnit.DocTest, except: [doctest: 1, doctest: 2]
      import ExUnit.TestHelpers.DocTestIsolated

      unquote(block)

      import ExUnit.TestHelpers.DocTestIsolated, except: [doctest: 1, doctest: 2]
      import ExUnit.DocTest, only: [doctest: 1, doctest: 2]
    end
  end

  defmacro doctest(module, options \\ []) do
    isolated_module = :"Isolated_#{System.unique_integer([:positive])}"
    async = Module.get_attribute(__CALLER__.module, :__doctest_isolate_async__)

    quote bind_quoted: binding() do
      defmodule isolated_module  do
        use ExUnit.Case, async: async
        ExUnit.DocTest.doctest(module, options)
      end

      :code.purge(isolated_module)
    end
  end
end
