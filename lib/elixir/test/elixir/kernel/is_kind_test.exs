defmodule Kernel.IsKindTest do
  use ExUnit.Case, async: true

  @kinds [
    :arity,
    :atom,
    :binary,
    :bitstring,
    :boolean,
    :byte,
    :char,
    :empty_list,
    :even,
    :false,
    :falsey,
    :float,
    :fun,
    {:fun, :arity},
    :function,
    {:function, :arity},
    :identifier,
    :integer,
    :list,
    {:list, :length},
    :map,
    {:map, :size},
    :mfa,
    :module,
    :neg_float,
    :neg_integer,
    :negative,
    :nil,
    :node,
    :non_neg_integer,
    :nonempty_list,
    :number,
    :odd,
    :pid,
    :port,
    :pos_float,
    :pos_integer,
    :positive,
    :record,
    {:record, :size},
    {:record, :kind},
    :reference,
    :timeout,
    :true,
    :truthy,
    :tuple,
    {:tuple, :size},
    :zero,
    :zero_or_neg_float,
    :zero_or_neg_integer,
    :zero_or_negative,
    :zero_or_pos_float,
    :zero_or_pos_integer,
    :zero_or_positive,
    {:==, :value}, {:eq, :value},
    {:!=, :value}, {:not_eq, :value},
    {:===, :value}, {:strict_eq, :value},
    {:!==, :value}, {:not_strict_eq, :value},
    {:<, :value}, {:lt, :value},
    {:<=, :value}, {:lt_eq, :value},
    {:>, :value}, {:gt, :value},
    {:>=, :value}, {:gt_eq, :value},
  ]

  for kind <- @kinds do
    case kind do
      k when is_atom(k) ->
        def test_is_kind(term, unquote(k)) when is_kind(term, unquote(k)),
          do: true
        def test_is(term, unquote(k)) when term is unquote(k),
          do: true
        def test_is_not(term, unquote(k)) when term is_not unquote(k),
          do: true

      {k, _v} ->
        def test_is_kind(term, {unquote(k), value}) when is_kind(term, {unquote(k), value}),
          do: true
        def test_is(term, {unquote(k), value}) when term is {unquote(k), value},
          do: true
        def test_is_not(term, {unquote(k), value}) when term is_not {unquote(k), value},
          do: true
    end
  end

  def test_are_any_number(item1, item2 \\ self(), item3 \\ self())
  def test_are_any_number(item1, item2, item3) when [item1, item2, item3] are_any [:arity, :integer, :float],
    do: true

  def test_is_kind(term, number) when is_number(number),
    do: is_kind(term, number)
  def test_is_kind(_, _),
    do: false

  def test_is(term, :number) when term is :number, do: true
  def test_is(term, :integer) when term is :integer, do: true
  def test_is(term, :float) when term is :float, do: true
  def test_is(_, _), do: false

  def test_is_not(term, :number) when term is_not :number, do: true
  def test_is_not(term, :integer) when term is_not :integer, do: true
  def test_is_not(term, :float) when term is_not :float, do: true
  def test_is_not(_, _), do: false

  def test_is_any(term, number) when is_number(number),
    do: term is number
  def test_is_any(_, _),
    do: false

  test "is/2" do
    assert 10 is :number == true
    assert 10 is [:number, :integer] == true
    assert [] is [:list, :empty_list] == true
    assert_raise ArgumentError, fn ->
      10 is []
    end

    refute [10] is :number == true
    refute [10] is [:number] == true
    refute [10] is [:number, :list] == true
    assert_raise ArgumentError, fn ->
      [] is [] == true
    end

    assert test_is(10, :number)
    refute test_is(10, [])
    refute test_is(10, [:number])
    refute test_is([10], :number) == true
    # refute test_is([10], [:number]) == true
    # refute test_is([10], [:number, :list]) == true
    refute test_is([], []) == true
  end

  test "are/2" do
    assert [10, 20] are :number == true
    assert [10, 20] are [:number, :integer] == true

    # assert test_are([10, 20], :number) == true
    # assert test_are([10, 20], [:number, :integer]) == true
  end

  test "is_not/2" do
    assert "10" is_not :number == true
    assert "10" is_not [:number, :integer] == true

    assert test_is_not("10", :number)
    assert test_is_not("10", :integer)
    refute test_is_not(10.0, :float)
  end


  # def test_are_not(term, kind) when term is [:number, :integer],
  #   do: true

  test "are_not/2" do
    assert ["10", :"20"] are_not :number == true
    assert ["10", :"20"] are_not [:number, :integer] == true
    assert [1, 3, 15, -1] are_not [:even, :positive] == false

    # assert test_are_not(["10", :"20"], :number) == true
    # assert test_are_not(["10", :"20"], [:number, :integer]) == true
    # assert test_are_not([1, 3, 15, -1], [:even, :positive]) == false
  end

  test "is_any/2" do
    assert 12.34 is_any [:bitstring, :float, :list] == true
    assert 12.34 is_any [:float] == true
    assert_raise ArgumentError, fn ->
      12.34 is_any []
    end
    assert_raise ArgumentError, fn ->
      [] is_any []
    end
  end

  test "are_any/2" do
    assert [1, 3, 15, -2] are_any [:odd, :negative] == true
    assert [10, 0, -10.0] are_any [:positive, :negative, :zero] == true
    assert [10, "string", [:foo]] are_any [:number, :bitstring, :nonempty_list] == true
  end

  test "is_kind/2" do
    # TODO: identifier, port, reference

    ##############################
    # Basic and built-in types
    ##############################

    # :arity
    assert is_kind(0, :arity) == true
    assert is_kind(255, :arity) == true
    refute is_kind(256, :arity)
    refute is_kind(-10, :arity)
    refute is_kind(1.0, :arity)

    # :atom
    assert is_kind(:foo, :atom) == true
    refute is_kind(":foo", :atom)

    # :byte
    assert is_kind(0, :byte) == true
    assert is_kind(123, :byte) == true
    assert is_kind(255, :byte) == true
    refute is_kind(300, :byte)
    refute is_kind(-1, :byte)

    # :binary
    assert is_kind("binary", :binary) == true
    refute is_kind(<<1::3>>, :binary)

    # :bitstring
    assert is_kind("binary", :bitstring) == true
    assert is_kind(<<1::3>>, :bitstring) == true
    refute is_kind([132], :bitstring)

    # :boolean
    assert is_kind(true, :boolean) == true
    assert is_kind(false, :boolean) == true
    refute is_kind(nil, :boolean)

    # :char
    assert is_kind(0, :char) == true
    assert is_kind(123, :char) == true
    assert is_kind(0x10FFFF, :char) == true
    refute is_kind(0x110000, :char)
    refute is_kind(-1, :char)

    # :fun | :function
    assert is_kind(fn -> :ok end, :fun) == true
    assert is_kind(fn -> :ok end, :function) == true

    # {:fun, arity} | {:function, arity}
    assert is_kind(fn -> :ok end, :fun) == true
    assert is_kind(fn -> :ok end, :function) == true
    assert is_kind(&(&1), {:fun, 1}) == true
    assert is_kind(&(&1), {:function, 1}) == true
    assert is_kind(&({&1, &2}), {:fun, 2})
    assert is_kind(&({&1, &2}), {:function, 2})
    refute is_kind(&({&1, &2}), {:fun, 3})
    refute is_kind(&({&1, &2}), {:function, 3})

    # :list
    assert is_kind([], :list) == true
    assert is_kind([1, 2], :list) == true
    assert is_kind([1 | 2], :list) == true
    assert is_kind('abcd', :list) == true
    assert is_kind('', :list) == true
    refute is_kind("bitstring", :list)

    # {:list, length}
    assert is_kind([], {:list, 0}) == true
    assert is_kind([1, 2], {:list, 2}) == true
    # length doesn't work with improper lists
    assert_raise ArgumentError, fn ->
      refute is_kind([1 | 2], {:list, 2})
    end
    assert is_kind('abcd', {:list, 4}) == true
    assert is_kind('', {:list, 0}) == true
    refute is_kind("bitstring", {:list, 9})

    # :map
    assert is_kind(%{a: 1}, :map) == true
    assert is_kind(%{}, :map) == true
    refute is_kind({:a, 1}, :map)

    # {:map, size}
    assert is_kind(%{a: 1, b: 2, c: 3}, {:map, 3}) == true
    refute is_kind(%{a: 1, b: 2, c: 3}, {:map, 4}) == true
    assert is_kind(%{}, {:map, 0}) == true
    refute is_kind({:a, 1}, {:map, 1})

    # :nonempty_list
    assert is_kind([1], :nonempty_list) == true
    refute is_kind([], :nonempty_list)
    refute is_kind({1, 2}, :nonempty_list)

    # :tuple
    assert is_kind({}, :tuple) == true
    assert is_kind({1, 2}, :tuple) == true
    refute is_kind([1, 2], :tuple)

    # {:tuple, size}
    assert is_kind({}, {:tuple, 0}) == true
    assert is_kind({1, 2}, {:tuple, 2}) == true
    refute is_kind([1, 2], {:tuple, 2})

    # :number
    assert is_kind(1,    :number) == true
    assert is_kind(1.2,  :number) == true
    assert is_kind(-1,   :number) == true
    assert is_kind(-1.2, :number) == true
    assert is_kind(-0,   :number) == true
    assert is_kind(-0,   :number) == true
    refute is_kind("12",   :number)

    # :float
    assert is_kind(-0.0, :float) == true
    assert is_kind(1.2,  :float) == true
    refute is_kind(1,    :float)

    # :integer
    assert is_kind(0, :integer) == true
    assert is_kind(12, :integer) == true
    assert is_kind(-12, :integer) == true
    refute is_kind(-12.0, :integer)

    # :pos_integer
    assert is_kind(12, :pos_integer) == true
    assert is_kind(1, :pos_integer) == true
    refute is_kind(-12, :pos_integer)
    refute is_kind(0, :pos_integer)
    refute is_kind(12.0, :pos_integer)

    # :neg_integer
    assert is_kind(-12, :neg_integer) == true
    assert is_kind(-1, :neg_integer) == true
    refute is_kind(1, :neg_integer)
    refute is_kind(0, :neg_integer)
    refute is_kind(-12.0, :neg_integer)

    # :non_neg_integer | :zero_or_pos_integer
    assert is_kind(0, :non_neg_integer) == true
    assert is_kind(0, :zero_or_pos_integer) == true
    assert is_kind(2, :non_neg_integer) == true
    assert is_kind(2, :zero_or_pos_integer) == true
    assert is_kind(1, :zero_or_pos_integer) == true
    refute is_kind(-1, :zero_or_pos_integer)
    refute is_kind(12.0, :zero_or_pos_integer)

    # TODO: :identifier

    # :mfa
    assert is_kind({__MODULE__, :foo, 1}, :mfa) == true
    refute is_kind({__MODULE__, :foo, 1000}, :mfa) == true
    refute is_kind({__MODULE__, &(&1), 1}, :mfa)
    refute is_kind(123, :mfa)

    # :module
    assert is_kind(__MODULE__, :module) == true
    assert is_kind(:foo, :module) == true
    assert is_kind({}, :module) == true

    # :node
    assert is_kind(:foo, :node) == true
    refute is_kind(":foo", :node)

    # :pid
    assert is_kind(self(), :pid) == true
    refute is_kind(:atom, :pid)

    # TODO: :port
    # TODO: :reference

    # :timeout
    assert is_kind(:infinity, :timeout) == true
    assert is_kind(0, :timeout) == true
    assert is_kind(1000000000, :timeout) == true
    refute is_kind(:eternity, :timeout)
    refute is_kind(10.1, :timeout)
    refute is_kind(-1, :timeout)

    ##########################################
    # Additional: Derived from is_* functions/macros
    ##########################################

    # :even
    assert is_kind(2, :even) == true
    assert is_kind(-2, :even) == true
    assert is_kind(0, :even) == true
    refute is_kind(1, :even)
    refute is_kind(-11, :even)
    refute is_kind(2.0, :even)

    # :odd
    assert is_kind(1, :odd) == true
    assert is_kind(-1, :odd) == true
    assert is_kind(0, :odd) == false
    assert is_kind(20, :odd) == false
    refute is_kind(2, :odd)
    refute is_kind(-12, :odd)
    refute is_kind(1.0, :odd)

    # :nil
    assert is_kind(nil, :nil) == true
    refute is_kind(false, :nil)

    # :record
    record = {User, "john", 27}
    assert is_kind(record, :record) == true
    assert is_kind([], :record) == false
    refute is_kind({"john", 27}, :record)
    assert is_kind(123, :record) == false

    # {:record, size}
    record = {User, "john", 27}
    assert is_kind(record, {:record, 3}) == true
    assert is_kind(record, {:record, 2}) == false
    assert_raise ArgumentError, fn ->
      is_kind({}, {:record, 0})
    end
    assert is_kind({"foo"}, {:record, 1}) == false

    # {:record, kind}
    record = {User, "john", 27}
    assert is_kind(record, {:record, User}) == true
    assert_raise ArgumentError, fn ->
      is_kind(record, {:record, "user"}) == false
    end
    assert is_kind({}, {:record, nil}) == false
    assert_raise ArgumentError, fn ->
      is_kind({"foo"}, {:record, "foo"}) == false
    end
    assert is_kind({"foo"}, {:record, :foo}) == false

    ##############################
    # Additional: Convenience
    ##############################

    # :empty_list
    assert is_kind([], :empty_list) == true
    assert is_kind('', :empty_list) == true
    refute is_kind([2], :empty_list)
    refute is_kind('2', :empty_list)

    # :falsey
    assert is_kind(nil, :falsey) == true
    assert is_kind(false, :falsey) == true
    refute is_kind(0, :falsey)
    refute is_kind([], :falsey)

    # :truthy
    assert is_kind(true, :truthy) == true
    assert is_kind(0, :truthy) == true
    assert is_kind([], :truthy) == true
    refute is_kind(nil, :truthy)
    refute is_kind(false, :truthy)

    # :positive
    assert is_kind(1, :positive) == true
    assert is_kind(1.1, :positive) == true
    assert is_kind(0.1, :positive) == true
    refute is_kind(0, :positive)
    refute is_kind(-1, :positive)

    # :negative
    assert is_kind(-1, :negative) == true
    assert is_kind(-1.1, :negative) == true
    assert is_kind(-0.1, :negative) == true
    refute is_kind(0, :negative)
    refute is_kind(1, :negative)

    # :pos_float
    assert is_kind(1.1, :pos_float) == true
    assert is_kind(0.1, :pos_float) == true
    refute is_kind(1, :pos_float)
    refute is_kind(0.0, :pos_float)
    refute is_kind(-1.1, :pos_float)

    # :neg_float
    assert is_kind(-1.1, :neg_float) == true
    assert is_kind(-0.1, :neg_float) == true
    refute is_kind(-1, :neg_float)
    refute is_kind(0.0, :neg_float)
    refute is_kind(1.1, :neg_float)

    # :zero
    assert is_kind(0, :zero) == true
    assert is_kind(0.0, :zero) == true
    refute is_kind(0.0001, :zero)
    refute is_kind("0", :zero)

    # :zero_or_positive
    assert is_kind(0, :zero_or_positive) == true
    assert is_kind(0.0, :zero_or_positive) == true
    assert is_kind(0.0001, :zero_or_positive) == true
    assert is_kind(1000, :zero_or_positive) == true
    assert is_kind(1000.1, :zero_or_positive) == true
    refute is_kind("0", :zero_or_positive)

    # :zero_or_pos_float
    assert is_kind(0.0, :zero_or_pos_float) == true
    assert is_kind(0.0001, :zero_or_pos_float) == true
    assert is_kind(1000.1, :zero_or_pos_float) == true
    refute is_kind(0, :zero_or_pos_float)
    refute is_kind(1000, :zero_or_pos_float)
    refute is_kind("0", :zero_or_pos_float)

    # :zero_or_negative
    assert is_kind(0, :zero_or_negative) == true
    assert is_kind(0.0, :zero_or_negative) == true
    assert is_kind(-1.1, :zero_or_negative) == true
    assert is_kind(-0.01, :zero_or_negative) == true
    assert is_kind(-1, :zero_or_negative) == true
    refute is_kind(1.1, :zero_or_negative) == true
    refute is_kind("0", :zero_or_negative)

    # :zero_or_neg_integer
    assert is_kind(0, :zero_or_neg_integer) == true
    assert is_kind(-1, :zero_or_neg_integer) == true
    refute is_kind(0.0, :zero_or_neg_integer)
    refute is_kind(-0.01, :zero_or_neg_integer)
    refute is_kind(0.0001, :zero_or_neg_integer)
    refute is_kind("0", :zero_or_neg_integer)

    # :zero_or_neg_float
    assert is_kind(0.0, :zero_or_neg_float) == true
    assert is_kind(-0.01, :zero_or_neg_float) == true
    assert is_kind(-1.0, :zero_or_neg_float) == true
    refute is_kind(0, :zero_or_neg_float)
    refute is_kind(0.0001, :zero_or_neg_float)
    refute is_kind("0", :zero_or_neg_float)

    #############################
    # Additional: Comparison
    #############################

    assert is_kind(42, {:==, 42})
    assert is_kind(42, {:==, 42.0})
    refute is_kind(42, {:==, 42.1})

    refute is_kind(42, {:!=, 42})
    refute is_kind(42, {:!=, 42.0})
    assert is_kind(42, {:!=, 42.1})

    assert is_kind(42, {:===, 42})
    refute is_kind(42, {:===, 42.0})
    refute is_kind(42, {:===, 42.1})

    refute is_kind(42, {:!==, 42})
    assert is_kind(42, {:!==, 42.0})
    assert is_kind(42, {:!==, 42.1})

    refute is_kind(42, {:<, 41})
    refute is_kind(42, {:<, 42})
    assert is_kind(42, {:<, 43})

    refute is_kind(42, {:<=, 41})
    assert is_kind(42, {:<=, 42})
    assert is_kind(42, {:<=, 43})

    assert is_kind(42, {:>, 41})
    refute is_kind(42, {:>, 42})
    refute is_kind(42, {:>, 43})

    assert is_kind(42, {:>=, 41})
    assert is_kind(42, {:>=, 42})
    refute is_kind(42, {:>=, 43})

    #############################
    # Additional: Numbers
    #############################

    # :true
    assert is_kind(true, true) == true
    refute is_kind(nil, true)
    refute is_kind(1, true)
    refute is_kind(false, true)

    # :false
    assert is_kind(false, false) == true
    refute is_kind(nil, false)
    refute is_kind(1, false)
    refute is_kind(true, false)

    # integers (ie. 0, 1, 2, 3)
    x = 42
    assert is_kind(x, 42) == true
    refute is_kind(x, 42.0)
    refute is_kind(x, 1)
    assert is_kind(0x10AAAA, 1092266) == true

    # floats (ie. 4.5, 6.78)
    x = 42.0
    assert is_kind(x, 42.0) == true
    refute is_kind(x, 42)
    refute is_kind(x, 1)
    refute is_kind(x, 1.0)
  end

  test "test_is_kind/2" do
    # TODO: add tests for :identifier, :port, :reference

    ##############################
    # Basic and built-in types
    ##############################

    # :arity
    assert test_is_kind(0, :arity) == true
    assert test_is_kind(255, :arity) == true
    refute test_is_kind(256, :arity)
    refute test_is_kind(-10, :arity)
    refute test_is_kind(1.0, :arity)

    # :atom
    assert test_is_kind(:foo, :atom) == true
    refute test_is_kind(":foo", :atom)

    # :byte
    assert test_is_kind(0, :byte) == true
    assert test_is_kind(123, :byte) == true
    assert test_is_kind(255, :byte) == true
    refute test_is_kind(300, :byte)
    refute test_is_kind(-1, :byte)

    # :binary
    assert test_is_kind("binary", :binary) == true
    refute test_is_kind(<<1::3>>, :binary)

    # :bitstring
    assert test_is_kind("binary", :bitstring) == true
    assert test_is_kind(<<1::3>>, :bitstring) == true
    refute test_is_kind([132], :bitstring)

    # :boolean
    assert test_is_kind(true, :boolean) == true
    assert test_is_kind(false, :boolean) == true
    refute test_is_kind(nil, :boolean)

    # :char
    assert test_is_kind(0, :char) == true
    assert test_is_kind(123, :char) == true
    assert test_is_kind(0x10FFFF, :char) == true
    refute test_is_kind(0x110000, :char)
    refute test_is_kind(-1, :char)

    # :fun | :function
    assert test_is_kind(fn -> :ok end, :fun) == true
    assert test_is_kind(fn -> :ok end, :function) == true

    # {:fun, arity} | {:function, arity}
    assert test_is_kind(fn -> :ok end, :fun) == true
    assert test_is_kind(fn -> :ok end, :function) == true
    assert test_is_kind(&(&1), {:fun, 1}) == true
    assert test_is_kind(&(&1), {:function, 1}) == true
    assert test_is_kind(&({&1, &2}), {:fun, 2})
    assert test_is_kind(&({&1, &2}), {:function, 2})
    refute test_is_kind(&({&1, &2}), {:fun, 3})
    refute test_is_kind(&({&1, &2}), {:function, 3})

    # :list
    assert test_is_kind([], :list) == true
    assert test_is_kind([1, 2], :list) == true
    assert test_is_kind([1 | 2], :list) == true
    assert test_is_kind('abcd', :list) == true
    assert test_is_kind('', :list) == true
    refute test_is_kind("bitstring", :list)

    # {:list, length}
    assert test_is_kind([], {:list, 0}) == true
    assert test_is_kind([1, 2], {:list, 2}) == true
    # length doesn't work with improper lists
    refute test_is_kind([1 | 2], {:list, 2})
    assert test_is_kind('abcd', {:list, 4}) == true
    assert test_is_kind('', {:list, 0}) == true
    refute test_is_kind("bitstring", {:list, 9})

    # :map
    assert test_is_kind(%{a: 1}, :map) == true
    assert test_is_kind(%{}, :map) == true
    refute test_is_kind({:a, 1}, :map)

    # {:map, size}
    assert test_is_kind(%{a: 1, b: 2, c: 3}, {:map, 3}) == true
    refute test_is_kind(%{a: 1, b: 2, c: 3}, {:map, 4}) == true
    assert test_is_kind(%{}, {:map, 0}) == true
    refute test_is_kind({:a, 1}, {:map, 1})

    # :nonempty_list
    assert test_is_kind([1], :nonempty_list) == true
    refute test_is_kind([], :nonempty_list)
    refute test_is_kind({1, 2}, :nonempty_list)

    # :tuple
    assert test_is_kind({}, :tuple) == true
    assert test_is_kind({1, 2}, :tuple) == true
    refute test_is_kind([1, 2], :tuple)

    # {:tuple, size}
    assert test_is_kind({}, {:tuple, 0}) == true
    assert test_is_kind({1, 2}, {:tuple, 2}) == true
    refute test_is_kind([1, 2], {:tuple, 2})

    # :number
    assert test_is_kind(1,    :number) == true
    assert test_is_kind(1.2,  :number) == true
    assert test_is_kind(-1,   :number) == true
    assert test_is_kind(-1.2, :number) == true
    assert test_is_kind(-0,   :number) == true
    assert test_is_kind(-0,   :number) == true
    refute test_is_kind("12",   :number)

    # :float
    assert test_is_kind(-0.0, :float) == true
    assert test_is_kind(1.2,  :float) == true
    refute test_is_kind(1,    :float)

    # :integer
    assert test_is_kind(0, :integer) == true
    assert test_is_kind(12, :integer) == true
    assert test_is_kind(-12, :integer) == true
    refute test_is_kind(-12.0, :integer)

    # :pos_integer
    assert test_is_kind(12, :pos_integer) == true
    assert test_is_kind(1, :pos_integer) == true
    refute test_is_kind(-12, :pos_integer)
    refute test_is_kind(0, :pos_integer)
    refute test_is_kind(12.0, :pos_integer)

    # :neg_integer
    assert test_is_kind(-12, :neg_integer) == true
    assert test_is_kind(-1, :neg_integer) == true
    refute test_is_kind(1, :neg_integer)
    refute test_is_kind(0, :neg_integer)
    refute test_is_kind(-12.0, :neg_integer)

    # :non_neg_integer | :zero_or_pos_integer
    assert test_is_kind(0, :non_neg_integer) == true
    assert test_is_kind(0, :zero_or_pos_integer) == true
    assert test_is_kind(2, :non_neg_integer) == true
    assert test_is_kind(2, :zero_or_pos_integer) == true
    assert test_is_kind(1, :zero_or_pos_integer) == true
    refute test_is_kind(-1, :zero_or_pos_integer)
    refute test_is_kind(12.0, :zero_or_pos_integer)

    # TODO: :identifier

    # :mfa
    assert test_is_kind({__MODULE__, :foo, 1}, :mfa) == true
    refute test_is_kind({__MODULE__, :foo, 1000}, :mfa) == true
    refute test_is_kind({__MODULE__, &(&1), 1}, :mfa)
    refute test_is_kind(123, :mfa)

    # :module
    assert test_is_kind(__MODULE__, :module) == true
    assert test_is_kind(:foo, :module) == true
    assert test_is_kind({}, :module) == true

    # :node
    assert test_is_kind(:foo, :node) == true
    refute test_is_kind(":foo", :node)

    # :pid
    assert test_is_kind(self(), :pid) == true
    refute test_is_kind(:atom, :pid)

    # TODO: :port
    # TODO: :reference

    # :timeout
    assert test_is_kind(:infinity, :timeout) == true
    assert test_is_kind(0, :timeout) == true
    assert test_is_kind(1000000000, :timeout) == true
    refute test_is_kind(:eternity, :timeout)
    refute test_is_kind(10.1, :timeout)
    refute test_is_kind(-1, :timeout)

    ##########################################
    # Additional: Derived from is_* functions/macros
    ##########################################

    # :even
    assert test_is_kind(2, :even) == true
    assert test_is_kind(-2, :even) == true
    assert test_is_kind(0, :even) == true
    refute test_is_kind(1, :even)
    refute test_is_kind(-11, :even)
    refute test_is_kind(2.0, :even)

    # :odd
    assert test_is_kind(1, :odd) == true
    assert test_is_kind(-1, :odd) == true
    assert test_is_kind(0, :odd) == false
    assert test_is_kind(20, :odd) == false
    refute test_is_kind(2, :odd)
    refute test_is_kind(-12, :odd)
    refute test_is_kind(1.0, :odd)

    # :nil
    assert test_is_kind(nil, :nil) == true
    refute test_is_kind(false, :nil)

    # :record
    record = {User, "john", 27}
    assert test_is_kind(record, :record) == true
    assert test_is_kind([], :record) == false
    refute test_is_kind({"john", 27}, :record)
    assert test_is_kind(123, :record) == false

    # {:record, size}
    record = {User, "john", 27}
    assert test_is_kind(record, {:record, 3}) == true
    assert test_is_kind(record, {:record, 2}) == false
    assert test_is_kind({}, {:record, 0}) == false
    assert test_is_kind({"foo"}, {:record, 1}) == false

    # {:record, kind}
    record = {User, "john", 27}
    assert test_is_kind(record, {:record, User}) == true
    refute test_is_kind(record, {:record, "user"})
    assert test_is_kind({}, {:record, nil}) == false
    refute test_is_kind({"foo"}, {:record, "foo"})
    assert test_is_kind({"foo"}, {:record, :foo}) == false

    ##############################
    # Additional: Convenience
    ##############################

    # :empty_list
    assert test_is_kind([], :empty_list) == true
    assert test_is_kind('', :empty_list) == true
    refute test_is_kind([2], :empty_list)
    refute test_is_kind('2', :empty_list)

    # :falsey
    assert test_is_kind(nil, :falsey) == true
    assert test_is_kind(false, :falsey) == true
    refute test_is_kind(0, :falsey)
    refute test_is_kind([], :falsey)

    # :truthy
    assert test_is_kind(true, :truthy) == true
    assert test_is_kind(0, :truthy) == true
    assert test_is_kind([], :truthy) == true
    refute test_is_kind(nil, :truthy)
    refute test_is_kind(false, :truthy)

    # :positive
    assert test_is_kind(1, :positive) == true
    assert test_is_kind(1.1, :positive) == true
    assert test_is_kind(0.1, :positive) == true
    refute test_is_kind(0, :positive)
    refute test_is_kind(-1, :positive)

    # :negative
    assert test_is_kind(-1, :negative) == true
    assert test_is_kind(-1.1, :negative) == true
    assert test_is_kind(-0.1, :negative) == true
    refute test_is_kind(0, :negative)
    refute test_is_kind(1, :negative)

    # :pos_float
    assert test_is_kind(1.1, :pos_float) == true
    assert test_is_kind(0.1, :pos_float) == true
    refute test_is_kind(1, :pos_float)
    refute test_is_kind(0.0, :pos_float)
    refute test_is_kind(-1.1, :pos_float)

    # :neg_float
    assert test_is_kind(-1.1, :neg_float) == true
    assert test_is_kind(-0.1, :neg_float) == true
    refute test_is_kind(-1, :neg_float)
    refute test_is_kind(0.0, :neg_float)
    refute test_is_kind(1.1, :neg_float)

    # :zero
    assert test_is_kind(0, :zero) == true
    assert test_is_kind(0.0, :zero) == true
    refute test_is_kind(0.0001, :zero)
    refute test_is_kind("0", :zero)

    # :zero_or_positive
    assert test_is_kind(0, :zero_or_positive) == true
    assert test_is_kind(0.0, :zero_or_positive) == true
    assert test_is_kind(0.0001, :zero_or_positive) == true
    assert test_is_kind(1000, :zero_or_positive) == true
    assert test_is_kind(1000.1, :zero_or_positive) == true
    refute test_is_kind("0", :zero_or_positive)

    # :zero_or_pos_float
    assert test_is_kind(0.0, :zero_or_pos_float) == true
    assert test_is_kind(0.0001, :zero_or_pos_float) == true
    assert test_is_kind(1000.1, :zero_or_pos_float) == true
    refute test_is_kind(0, :zero_or_pos_float)
    refute test_is_kind(1000, :zero_or_pos_float)
    refute test_is_kind("0", :zero_or_pos_float)

    # :zero_or_negative
    assert test_is_kind(0, :zero_or_negative) == true
    assert test_is_kind(0.0, :zero_or_negative) == true
    assert test_is_kind(-1.1, :zero_or_negative) == true
    assert test_is_kind(-0.01, :zero_or_negative) == true
    assert test_is_kind(-1, :zero_or_negative) == true
    refute test_is_kind(1.1, :zero_or_negative) == true
    refute test_is_kind("0", :zero_or_negative)

    # :zero_or_neg_integer
    assert test_is_kind(0, :zero_or_neg_integer) == true
    assert test_is_kind(-1, :zero_or_neg_integer) == true
    refute test_is_kind(0.0, :zero_or_neg_integer)
    refute test_is_kind(-0.01, :zero_or_neg_integer)
    refute test_is_kind(0.0001, :zero_or_neg_integer)
    refute test_is_kind("0", :zero_or_neg_integer)

    # :zero_or_neg_float
    assert test_is_kind(0.0, :zero_or_neg_float) == true
    assert test_is_kind(-0.01, :zero_or_neg_float) == true
    assert test_is_kind(-1.0, :zero_or_neg_float) == true
    refute test_is_kind(0, :zero_or_neg_float)
    refute test_is_kind(0.0001, :zero_or_neg_float)
    refute test_is_kind("0", :zero_or_neg_float)

    #############################
    # Additional: Comparison
    #############################

    assert test_is_kind(42, {:==, 42})
    assert test_is_kind(42, {:==, 42.0})
    refute test_is_kind(42, {:==, 42.1})

    refute test_is_kind(42, {:!=, 42})
    refute test_is_kind(42, {:!=, 42.0})
    assert test_is_kind(42, {:!=, 42.1})

    assert test_is_kind(42, {:===, 42})
    refute test_is_kind(42, {:===, 42.0})
    refute test_is_kind(42, {:===, 42.1})

    refute test_is_kind(42, {:!==, 42})
    assert test_is_kind(42, {:!==, 42.0})
    assert test_is_kind(42, {:!==, 42.1})

    refute test_is_kind(42, {:<, 41})
    refute test_is_kind(42, {:<, 42})
    assert test_is_kind(42, {:<, 43})

    refute test_is_kind(42, {:<=, 41})
    assert test_is_kind(42, {:<=, 42})
    assert test_is_kind(42, {:<=, 43})

    assert test_is_kind(42, {:>, 41})
    refute test_is_kind(42, {:>, 42})
    refute test_is_kind(42, {:>, 43})

    assert test_is_kind(42, {:>=, 41})
    assert test_is_kind(42, {:>=, 42})
    refute test_is_kind(42, {:>=, 43})

    #############################
    # Additional: Numbers
    #############################

    # :true
    assert test_is_kind(true, true) == true
    refute test_is_kind(nil, true)
    refute test_is_kind(1, true)
    refute test_is_kind(false, true)

    # :false
    assert test_is_kind(false, false) == true
    refute test_is_kind(nil, false)
    refute test_is_kind(1, false)
    refute test_is_kind(true, false)

    # integers
    x = 42
    assert test_is_kind(x, 42)
    refute test_is_kind(x, 42.0)
    refute test_is_kind(x, 1)
    assert test_is_kind(0x10AAAA, 1092266)

    # floats
    x = 42.0
    assert test_is_kind(x, 42.0) == true
    refute test_is_kind(x, 42)
    refute test_is_kind(x, 1)
    refute test_is_kind(x, 1.0)
  end


  test "using numbers" do
    kind = :number
    assert 10 is kind == true
    assert [10, 20] are kind == true
    assert "10" is_not kind == true
    assert ["10", :"20"] are_not kind == true

    list = [:bitstring, :float, :list]
    assert 12.34 is_any list == true

    list = [:odd, :negative]
    assert [1, 3, 15, -2] are_any list == true

    assert is_kind(10.0, kind) == true
    assert is_kind(0x10AAAA, 1092266) == true

    twenty_four = 24
    twenty_four_integer = 24
    twenty_four_float = 24.0
    assert is_kind(twenty_four, twenty_four_integer) == true
    assert is_kind(twenty_four, twenty_four_float) == false
    assert test_is_kind(twenty_four, twenty_four_integer) == true
    assert test_is_kind(twenty_four, twenty_four_float) == false
    assert test_is_kind(twenty_four, {:==, twenty_four_float})
    assert test_is_kind(twenty_four, {:===, twenty_four})
    refute test_is_kind(twenty_four, {:===, twenty_four_float})
    assert test_is_kind(twenty_four, {:!==, twenty_four_float})
    refute test_is_kind(twenty_four, {:<, 21})
    refute test_is_kind(twenty_four, {:<, twenty_four})
    assert test_is_kind(twenty_four, {:<=, twenty_four_float})
    assert test_is_kind(twenty_four, {:>, 23})
    refute test_is_kind(twenty_four, {:>, 25})
    assert test_is_kind(twenty_four, {:>=, twenty_four_float})
  end

  def test_is_kind_map(term) when is_kind(term, :map),
    do: true
  def test_is_kind_map(_),
    do: false

  def test_is_map(term) when term is :map and term is [:map, :map],
    do: true
  def test_is_map(_),
    do: false

  def test_is_not_map(term) when term is_not :map and term is_not [:map, :map],
    do: true
  def test_is_not_map(_),
    do: false

  def test_is_any_map(term) when term is_any [:pid, :map, :reference],
    do: true
  def test_is_any_map(_),
    do: false

  def test_are_map(item1, item2 \\ %{}, item3 \\ %{})
  def test_are_map(item1, item2, item3)
      when [item1, item2, item3] are :map and [item1, item2, item3] are [:map, :map],
    do: true
  def test_are_map(_, _, _),
    do: false

  def test_are_not_map(item1, item2 \\ self(), item3 \\ self())
  def test_are_not_map(item1, item2, item3)
      when [item1, item2, item3] are_not :map and [item1, item2, item3] are_not [:map, :map],
    do: true
  def test_are_not_map(_, _, _),
    do: false

  def test_are_any_map_tuple_list(item1, item2 \\ self(), item3 \\ self())
  def test_are_any_map_tuple_list(item1, item2, item3) when [item1, item2, item3] are_any [:map, :tuple, :list],
    do: true
  def test_are_any_map_tuple_list(_, _, _),
    do: false

  #************************

  def test_is_kind_map(term, size) when is_kind(term, {:map, size}),
    do: true
  def test_is_kind_map(_, _),
    do: false

  def test_is_map(term, size) when term is :map and term is [:map, {:map, size}],
    do: true
  def test_is_map(_, _),
    do: false

  def test_is_not_map(term, size) when term is_not :map and term is_not [:map, {:map, size}],
    do: true
  def test_is_not_map(_, _),
    do: false

  def test_is_any_map(term, size) when term is_any [:pid, {:map, size}, :reference],
    do: true
  def test_is_any_map(_, _),
    do: false

  def test_are_map(item1, item2, item3, size)
      when [item1, item2, item3] are {:map, size} and [item1, item2, item3] are [{:map, size}, {:map, size}],
    do: true
  def test_are_map(_, _, _, _),
    do: false

  def test_are_not_map(item1, item2, item3, size)
      when [item1, item2, item3] are_not {:map, size} and [item1, item2, item3] are_not [{:map, size}, {:map, size}],
    do: true
  def test_are_not_map(_, _, _, _),
    do: false

  def test_are_any_map(item1, item2, item3, size)
      when [item1, item2, item3] are_any [{:map, size}, {:map, size}] do
    true
  end

  def test_are_any_map(_, _, _, _),
    do: false

  #************************

  test "basic" do
    foo = :foo
    atom = :atom

    assert is_kind(foo, atom)
    assert is_kind(:foo, :atom)
    assert_raise ArgumentError, fn ->
      is_kind([:foo], [:atom])
    end
    assert_raise ArgumentError, fn ->
      arg = [:atom]
      is_kind([:foo, :bar], arg)
    end

    assert :foo is :atom
    assert ?r is :char
    assert false is [:atom, :boolean]
    assert :foo is [:atom, :atom]

    assert :foo is_not :char
    assert ?r is_not :boolean
    refute false is_not [:char, :boolean]
    assert 10 is_not [:atom, :boolean]

    assert :foo is_any [:char, :atom, :boolean]
    assert ?r is_any [:boolean, :char]
    refute 10 is_any [:boolean, :atom]

    assert [:foo] are :atom
    assert [:foo, :bar] are :atom
    assert [:foo] are [:atom, :atom]
    assert [:foo, :bar] are [:atom, :atom]
    refute [:foo, :bar, 1] are [:atom, :atom]
    assert [?r, ?z] are :char
    assert [30, 40] are [gt: 29, lt: 41]
    assert_raise ArgumentError, fn ->
      false are [:atom, :boolean]
    end

    assert [10] are_not :atom
    refute [:foo] are_not :atom
    refute [:foo, :bar] are_not :atom
    refute [:foo] are_not [:atom, :atom]
    assert [:foo, :bar] are_not [:char, :boolean]
    refute [:foo, :bar] are_not [:atom, :boolean]
    assert [?r, ?z] are_not :atom
    refute [?r, ?z] are_not :char
    assert_raise ArgumentError, fn ->
      false are_not [:atom, :boolean]
    end

    assert [:foo] are_any [:atom, :char]
    assert [:foo, 10] are_any [:atom, :char]
    refute [:foo, :bar, 1] are_any [:boolean]
    assert [?r, ?z], [:char]
    assert_raise ArgumentError, fn ->
      false are_any [:atom, :boolean]
    end
    assert_raise ArgumentError, fn ->
      [false] are_any :atom
    end
  end

  test "guards" do
    foo = :foo
    number = 10
    map = %{a: 1, b: 2, c: 3}
    empty_map = %{}

    assert test_is_kind_map(map)
    assert test_is_kind_map(empty_map)
    assert test_is_kind_map(%{a: 1, b: 2, c: 3})
    assert test_is_kind_map(%{})

    assert test_is_map(map)
    assert test_is_map(%{a: 1, b: 2, c: 3})
    assert test_is_map(map, 3)
    assert test_is_map(%{a: 1, b: 2, c: 3}, 3)
    refute test_is_map(map, 2)
    refute test_is_map(%{a: 1, b: 2, c: 3}, 2)
    assert test_is_map(%{})
    assert test_is_map(%{}, 0)
    refute test_is_map(1)

    assert test_is_not_map(foo)
    assert test_is_not_map(:foo)
    refute test_is_not_map(map)
    refute test_is_not_map(%{a: 1, b: 2, c: 3})

    assert test_is_any_map(map)
    assert test_is_any_map(%{a: 1})
    assert test_is_any_map(%{})
    refute test_is_any_map(100)
    refute test_is_any_map({})

    assert test_is_any_map(map, 3)
    assert test_is_any_map(%{a: 1}, 1)
    assert test_is_any_map(%{}, 0)
    refute test_is_any_map(100, 0)
    refute test_is_any_map({}, 0)

    assert test_are_map(map)
    assert test_are_map(%{a: 1, b: 2, c: 3})
    assert test_are_map(%{})
    assert test_are_map(map, map, map, 3)
    refute test_are_map(map, %{}, map, 3)
    assert test_are_map(%{a: 1, b: 2, c: 3}, %{a: 1, b: 2, c: 3}, %{a: 1, b: 2, c: 3}, 3)
    refute test_are_map(%{a: 1, b: 2, c: 3}, %{}, %{a: 1, b: 2, c: 3}, 3)
    {a, b, c} = {true, false, true}
    refute test_are_map(a, b, c)
    refute test_are_map(a, b, c, 0)
    {a, b, c} = {%{}, %{}, %{}}
    assert test_are_map(a, b, c)
    assert test_are_map(a, b, c, 0)

    assert test_are_not_map(number)
    assert test_are_not_map(10)
    assert test_are_not_map(10, self())
    assert test_are_not_map(10, self(), {})
    assert test_are_not_map(10, self(), {}, 3)

    assert test_are_any_map_tuple_list({:tuple}, map, [:bar])
    assert test_are_any_map_tuple_list(%{a: 1}, %{a: 1, b: 2}, %{a: 1, b: 2, c: 3})
    assert test_are_any_map_tuple_list(%{}, %{}, %{})
    refute test_are_any_map_tuple_list(100)
    refute test_are_any_map_tuple_list(100, {}, [])

    assert test_are_any_map(map, map, map, 3)
    refute test_are_any_map(map, map, map, 2)
    refute test_are_any_map(:foo, map, :bar, 2)
    assert test_are_any_map(%{a: 1, b: 2, c: 3}, %{a: 1, b: 2, c: 3}, %{a: 1, b: 2, c: 3}, 3)
    assert test_are_any_map(%{}, %{}, %{}, 0)
    refute test_are_any_map(100, {}, [], 0)
  end

  # TODO: write tests for errors

  # TODO: write test args not given at compile time
  test "non-compile time provided args" do
    map = %{a: 1, b: 2, c: 3}
    tuple = {:a, :b, :c}

    list_of_maps = [map, map]
    list_of_tuples = [tuple, tuple]
    list_of_maps_tuples = [map, tuple]

    kind_map = :map
    list_of_kinds_map = [:map]
    list_of_kinds_map_tuple = [:map, :tuple]
    kind_tuple = :tuple

    # module
    assert is_kind(map, kind_map)
    assert map is kind_map
    assert tuple is_not kind_map
    assert tuple is_any [kind_map, kind_tuple]
    assert tuple is_any list_of_kinds_map_tuple
    assert list_of_maps are kind_map
    assert list_of_maps are list_of_kinds_map
    assert list_of_tuples are_not kind_map
    assert list_of_tuples are_not list_of_kinds_map
    assert [tuple, tuple] are_not [kind_map, kind_map]
    assert [tuple, map] are_any [kind_map, kind_tuple]
    assert list_of_maps_tuples are_any list_of_kinds_map_tuple
  end

  def test_guard_is_kind(term, 10) when is_kind(term, 10), do: :ten
  def test_guard_is_kind(term, :map) when is_kind(term, :map), do: true
  def test_guard_is_kind(term, {:map, size}) when is_kind(term, {:map, size}), do: true
  def test_guard_is_kind(term, :record) when is_kind(term, :record), do: true
  def test_guard_is_kind(term, {:record, record_kind}) when is_kind(term, {:record, record_kind}), do: true
  def test_guard_is_kind(term, {:record, size}) when is_kind(term, {:record, size}), do: true
  def test_guard_is_kind(_, _), do: false

  def test_guard_is_record(term) when term is :record, do: true
  def test_guard_is_record(_), do: false
  def test_guard_is_record(term, record_kind) when term is {:record, record_kind}, do: true
  def test_guard_is_record(term, size) when term is {:record, size}, do: true
  def test_guard_is_record(_, _), do: false

  def test_guard_is_not_record(term) when term is_not :record, do: true
  def test_guard_is_not_record(_), do: false
  def test_guard_is_not_record(term, record_kind) when term is_not {:record, record_kind}, do: true
  def test_guard_is_not_record(term, size) when term is_not {:record, size}, do: true
  def test_guard_is_not_record(_, _), do: false

  def test_guard_is_any_record(term) when term is_any [:record, :pid], do: true
  def test_guard_is_any_record(_), do: false
  def test_guard_is_any_record(term, record_kind) when term is [{:record, record_kind}, :pid], do: true
  def test_guard_is_any_record(term, size) when term is [{:record, size}, :pid], do: true
  def test_guard_is_any_record(_, _), do: false

  def test_guard_are_record(item1, item2, item3) when [item1, item2,  item3] are :record, do: true
  def test_guard_are_record(_, _, _), do: false

  def test_guard_are_not_record(item1, item2, item3) when [item1, item2,  item3] are_not :record, do: true
  def test_guard_are_not_record(_, _, _), do: false

  def test_guard_are_any_record(item1, item2, item3) when [item1, item2, item3] are_any [:record, :pid], do: true
  def test_guard_are_any_record(_), do: false
end
