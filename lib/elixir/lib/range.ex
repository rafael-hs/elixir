defmodule Range do
  @moduledoc """
  Defines a range.

  A range represents a discrete number of values where
  the first and last values are integers.

  Ranges can be either increasing (first <= last) or
  decreasing (first > last). Ranges are also always
  inclusive.

  A Range is represented internally as a struct. However,
  the most common form of creating and matching on ranges
  is via the `../2` macro, auto-imported from Kernel:

      iex> range = 1..3
      1..3
      iex> first..last = range
      iex> first
      1
      iex> last
      3

  A Range implements the Enumerable protocol, which means
  all of the functions in the Enum module is available:

      iex> range = 1..10
      1..10
      iex> Enum.reduce(range, 0, fn i, acc -> i * i + acc end)
      385
      iex> Enum.count(range)
      10
      iex> Enum.member?(range, 11)
      false
      iex> Enum.member?(range, 8)
      true

  """

  defstruct first: nil, last: nil

  @type t :: %Range{}
  @type t(first, last) :: %Range{first: first, last: last}

  @doc """
  Creates a new range.
  """
  @spec new(integer, integer) :: t
  def new(first, last) when is_integer(first) and is_integer(last) do
    %Range{first: first, last: last}
  end

  def new(first, last) do
    raise ArgumentError,
      "ranges (first..last) expect both sides to be integers, " <>
      "got: #{inspect first}..#{inspect last}"
  end

  @doc """
  Returns `true` if the given `term` is a range.

  It does not check if the range is valid.

  ## Examples

      iex> Range.range?(1..3)
      true

      iex> Range.range?(0)
      false

  """
  @spec range?(%Range{}) :: true
  @spec range?(term) :: false
  def range?(term)
  def range?(%Range{}), do: true
  def range?(_), do: false

  @doc """
  Determines if two ranges are congruent (i.e. that both cover the same range, regardless the order
  of their limits).

  ## Examples:

      iex> Range.congruent?(-1..5, 5..-1)
      true

      iex> Range.congruent?(-1..5, -1..5)
      true

      iex> Range.congruent?(1..5, 5..-1)
      false
  """
  @spec congruent?(t, t) :: boolean
  def congruent?(range1, range2)
  def congruent?(a..b, x..y) when (a == x and b == y) or (a == y and b == x),
    do: true
  def congruent?(_.._, _.._),
    do: false

  @doc """
  Determines if two ranges are contiguous.

  Note that the order of the limits does not matter to determine whether the ranges are contiguous
  or not (eg. `1..3` and `4..6` are contiguous, same as `3..1` and `4..6`).

  If the ranges overlap each other, it will return `false`.

  ## Examples

      iex> Range.contiguous?(1..4, 5..8)
      true

      iex> Range.contiguous?(1..4, 0..-4)
      true

      # Ranges overlap
      iex> Range.contiguous?(1..4, 0..1)
      false

      iex> Range.contiguous?(1..4, 6..8)
      false

      iex> Range.contiguous?(1..5, 4..8)
      false
  """
  @spec contiguous?(t, t) :: boolean
  def contiguous?(_.._ = range1, _.._ = range2) do
    a..b = sort(range1)
    x..y = sort(range2)
    if (b + 1 == x) or (y + 1 == a) do
      true
    else
      false
    end
  end

  @doc """
  Checks if `range1` and `range2` have no members in common.

  ## Examples

      iex> Range.disjoint?(1..2, 3..4)
      true
      iex> Range.disjoint?(1..2, 2..3)
      false

  """
  @spec disjoint?(t, t) :: boolean
  def disjoint?(a..b = range1, x.._ = range2) do
    not( (a in range2) or (b in range2) or (x in range1) )
  end

  @doc """
  Returns a range containing the range shared in common between `range1` and
  `range2`.
  Returns `nil` if there is no intersection.

  Note that the returned range, if any, will always be in ascending order (the
  first element is <= the last element).

  ## Examples

      iex> Range.intersection(1..5, 4..8)
      4..5

      iex> Range.intersection(-1..4, -3..8)
      -1..4

      iex> Range.intersection(1..4, 4..8)
      4..4

      iex> Range.intersection(5..5, 5..5)
      5..5

      iex> Range.intersection(1..5, 6..8)
      nil
  """
  @spec intersection(t, t) :: t | nil
  def intersection(range1, range2)
  def intersection(a..b = range1, x..y = _range2) when (a == x and b == y) or (a == y and b == x) do
    sort(range1)
  end

  def intersection(_.._ = range1, _.._ = range2) do
    if overlaps?(range1, range2) do
      a..b = sort(range1)
      x..y = sort(range2)
      max(a, x)..min(b, y)
    else
      nil
    end
  end

  @doc """
  Checks if `ranges` contains `value`.

  ## Examples

      iex> Range.member?(1..3, 2)
      true
      iex> Range.member?(1..3, 4)
      false
      iex> Range.member?(1..3, :foo)
      false
      iex> Range.member?(1..3, 1..3)
      false
  """
  @spec member?(t, term) :: boolean
  def member?(first..last = range, value) when is_integer(value) do
    if first <= last do
      first <= value and value <= last
    else
      last <= value and value <= first
    end
  end

  def member?(_.._, _value) do
    false
  end

  @doc """
  Determines if two ranges overlap each other.

  ## Examples

      iex> Range.overlaps?(1..5, 4..6)
      true

      iex> Range.overlaps?(1..5, 7..9)
      false

      iex> Range.overlaps?(5..1, 6..4)
      true
  """
  @spec overlaps?(t, t) :: boolean
  def overlaps?(a..b = range1, x.._ = range2) do
    (a in range2) or (b in range2) or (x in range1)
  end

  @doc """
  Swaps the order of the first and last limits in the range.

  ## Examples

    iex> Range.reverse(0..10)
    10..0

    iex> Range.reverse(10..0)
    0..10
  """
  @spec reverse(t) :: t
  def reverse(range)
  def reverse(first..last) do
    last..first
  end

  @doc """
  Returns the number of elements in `range`.

  ## Examples

      iex> Range.size(-5..5)
      11

      iex> Range.size(3..3)
      1
  """
  @spec size(t) :: pos_integer
  def size(range)
  def size(first..first),
    do: 1
  def size(first..last) when first < last,
    do: last - first + 1
  def size(first..last) when first > last,
    do: first - last + 1

  @doc """
  Sorts the `range` by the given `order`.

  `order` can take either `:ascending` or `:descending`.
  If no `order` is specified, `:ascending` order is used.

  ## Examples

      iex> Range.sort(0..10)
      0..10

      iex> Range.sort(10..0)
      0..10

      iex> Range.sort(0..10, :descending)
      10..0
  """
  @spec sort(t, :ascending | :descending) :: t
  def sort(range, order \\ :ascending)

  def sort(first..last, order)
  when (order == :ascending and first > last) or (order == :descending and last > first) do
    last..first
  end

  def sort(_first.._last = range, order) when order in [:ascending, :descending] do
    range
  end

  @doc """
  Checks if `range1`'s members are all contained in `range2`.

  This function checks if `range1` is a subset of `range2`.

  ## Examples

      iex> Range.subset?(1..2, 1..3)
      true
      iex> Range.subset?(1..3, 1..2)
      false

  """
  @spec subset?(t, t) :: boolean
  def subset?(a..b = _range1, range2) do
    (a in range2) and (b in range2)
  end

  @doc """
  Converts `range` to a list.

  ## Examples

      iex> Range.to_list(1..3)
      [1, 2, 3]

  """
  @spec to_list(t) :: list
  def to_list(_.._ = range) do
    Enum.to_list(range)
  end

  @doc """
  Returns the union of two ranges, or `nil` if there is no union.

  A union the represented by the smallest element and the biggest integers in
  both ranges, provided that the ranges overlap or are contiguous.

  Note that the returned range, if any, will be in ascending order.

  ## Examples

      iex> Range.union(1..3, 4..6)
      1..6

      iex> Range.union(1..5, 4..8)
      1..8

      iex> Range.union(3..6, 1..3)
      1..6
  """
  @spec union(t, t) :: t | nil
  def union(range1, range2)
  def union(a..b = range1, x..y = _range2) when (a == x and b == y) or (a == y and b == x) do
    sort(range1)
  end

  def union(_.._ = range1, _.._ = range2) do
    if overlaps?(range1, range2) or contiguous?(range1, range2) do
      a..b = sort(range1)
      x..y = sort(range2)
      min(a, x)..max(b, y)
    else
      nil
    end
  end
end

defimpl Enumerable, for: Range do
  def reduce(first .. last, acc, fun) do
    reduce(first, last, acc, fun, last >= first)
  end

  defp reduce(_x, _y, {:halt, acc}, _fun, _up) do
    {:halted, acc}
  end

  defp reduce(x, y, {:suspend, acc}, fun, up) do
    {:suspended, acc, &reduce(x, y, &1, fun, up)}
  end

  defp reduce(x, y, {:cont, acc}, fun, true) when x <= y do
    reduce(x + 1, y, fun.(x, acc), fun, true)
  end

  defp reduce(x, y, {:cont, acc}, fun, false) when x >= y do
    reduce(x - 1, y, fun.(x, acc), fun, false)
  end

  defp reduce(_, _, {:cont, acc}, _fun, _up) do
    {:done, acc}
  end

  def member?(range, value),
    do: {:ok, Range.member?(range, value)}

  def count(_.._ = range),
    do: {:ok, Range.size(range)}
end

defimpl Inspect, for: Range do
  import Inspect.Algebra

  def inspect(first .. last, opts) do
    concat [to_doc(first, opts), "..", to_doc(last, opts)]
  end
end
