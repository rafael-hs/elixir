Code.require_file "test_helper.exs", __DIR__

defmodule RangeTest do
  use ExUnit.Case, async: true

  doctest Range

  test "precedence" do
    assert Enum.to_list(1..3+2) == [1, 2, 3, 4, 5]
    assert 1..3 |> Enum.to_list == [1, 2, 3]
  end

  test "op" do
    assert (1..3).first == 1
    assert (1..3).last  == 3
  end

  test "range?" do
    assert Range.range?(1..3)
    refute Range.range?([1, 2, 3])
    refute Range.range?(0)
  end

  test "enum" do
    refute Enum.empty?(1..1)

    assert Enum.member?(1..3, 2)
    refute Enum.member?(1..3, 0)
    refute Enum.member?(1..3, 4)
    refute Enum.member?(3..1, 0)
    refute Enum.member?(3..1, 4)

    assert Enum.count(1..3) == 3
    assert Enum.count(3..1) == 3

    assert Enum.map(1..3, &(&1 * 2)) == [2, 4, 6]
    assert Enum.map(3..1, &(&1 * 2)) == [6, 4, 2]
  end

  test "inspect" do
    assert inspect(1..3) == "1..3"
    assert inspect(3..1) == "3..1"
  end

  test "integer only" do
    x = 1.0
    y = 3.0
    message = "ranges (first..last) expect both sides to be integers, got: 1.0..3.0"
    assert_raise ArgumentError, message, fn ->
      Enum.map(x..y, &(&1 * 2))
    end

    first = []
    last = []
    message = "ranges (first..last) expect both sides to be integers, got: []..[]"
    assert_raise ArgumentError, message, fn ->
      first..last
      Enum.map(first..last, &(&1))
    end
  end

  test "invalid arguments" do
    list = [1, 2, 3, 4]
    range = 5..8

    # arity 1
    for f <- [:reverse, :sort] do
      assert_raise FunctionClauseError, fn ->
        apply(Range, f, [list])
      end
    end

    # arity 2
    for f <- [:congruent?, :contiguous?, :intersection, :overlaps?, :union] do
      assert_raise FunctionClauseError, fn ->
        apply(Range, f, [list, range])
      end
      assert_raise FunctionClauseError, fn ->
        apply(Range, f, [range, list])
      end
      assert_raise FunctionClauseError, fn ->
        apply(Range, f, [list, list])
      end
    end
  end

  test "disjoint?/2" do
    assert Range.disjoint?(1..4, 5..8)
    refute Range.disjoint?(0..2, 1..4)

    # Touching
    refute Range.disjoint?(4..4, 4..4)
    refute Range.disjoint?(0..1, 1..4)
    refute Range.disjoint?(2..4, 4..2)

    # Not touching
    assert Range.disjoint?(1..4, 6..8)
    assert Range.disjoint?(1..4, -1..-4)
    assert Range.disjoint?(1..4, -4..-1)

    # Contiguous
    assert Range.disjoint?(1..4, 5..8)
    assert Range.disjoint?(1..4, 0..-4)
    assert Range.disjoint?(1..4, -4..0)
  end

  test "congruent?/2" do
    assert Range.congruent?(1..4, 1..4)
    assert Range.congruent?(1..4, 4..1)
    assert Range.congruent?(4..4, 4..4)
    assert Range.congruent?(4..4, 4..4)

    refute Range.congruent?(1..4, 5..8)
    refute Range.congruent?(1..4, -1..-4)
    refute Range.congruent?(1..4, 1..5)
    refute Range.congruent?(0..4, 1..5)
    refute Range.congruent?(4..4, 3..3)
  end

  test "contiguous?/2" do
    assert Range.contiguous?(1..4, 5..8)
    assert Range.contiguous?(1..4, 0..-4)
    assert Range.contiguous?(1..4, -4..0)

    # Overlaping ranges
    refute Range.contiguous?(4..4, 4..4)
    refute Range.contiguous?(0..1, 1..4)
    refute Range.contiguous?(0..2, 1..4)
    refute Range.contiguous?(2..4, 4..2)

    # Not touching
    refute Range.contiguous?(1..4, 6..8)
    refute Range.contiguous?(1..4, -1..-4)
    refute Range.contiguous?(1..4, -4..-1)
  end

  test "intersection/2" do
    assert Range.intersection(1..5, 4..8) == 4..5
    assert Range.intersection(5..1, 4..8) == 4..5
    assert Range.intersection(1..5, 8..4) == 4..5
    assert Range.intersection(5..1, 8..4) == 4..5

    assert Range.intersection(1..4, 4..8) == 4..4
    assert Range.intersection(4..1, 4..8) == 4..4
    assert Range.intersection(1..4, 8..4) == 4..4
    assert Range.intersection(4..1, 8..4) == 4..4

    assert Range.intersection(-1..4, -3..8) == -1..4
    assert Range.intersection(-1..4, -3..3) == -1..3

    # same ranges
    assert Range.intersection(4..4, 4..4) == 4..4
    assert Range.intersection(4..-1, 4..-1) == -1..4
    assert Range.intersection(-1..4, -1..4) == -1..4
    assert Range.intersection(4..-1, -1..4) == -1..4
    assert Range.intersection(-1..4, 4..-1) == -1..4

    # contiguous ranges
    assert Range.intersection(0..1, 2..3) == nil
    assert Range.intersection(0..1, 3..2) == nil
    assert Range.intersection(1..1, 2..2) == nil

    # Not touching
    assert Range.intersection(1..4, 6..8) == nil
    assert Range.intersection(1..4, -1..-4) == nil
    assert Range.intersection(1..4, -4..-1) == nil
  end

  test "member?/1" do
    assert Range.member?(1..3, 1)
    assert Range.member?(1..3, 2)
    assert Range.member?(1..3, 3)
    refute Range.member?(1..3, 0)
    refute Range.member?(1..3, 4)
    refute Range.member?(3..1, 0)
    refute Range.member?(3..1, 4)
    
    assert Range.member?(3..3, 3)
    refute Range.member?(3..3, 4)

    refute Range.member?(1..3, :foo)
    refute Range.member?(1..3, 1..3)
    refute Range.member?(1..3, 2..2)
  end

  test "overlaps?/2" do
    assert Range.overlaps?(4..4, 4..4)
    assert Range.overlaps?(0..1, 1..4)
    assert Range.overlaps?(0..2, 1..4)
    assert Range.overlaps?(2..4, 4..2)

    # Contiguous
    refute Range.overlaps?(1..4, 5..8)
    refute Range.overlaps?(1..4, 0..-4)
    refute Range.overlaps?(1..4, -4..0)

    # Not touching
    refute Range.overlaps?(1..4, 6..8)
    refute Range.overlaps?(1..4, -1..-4)
    refute Range.overlaps?(1..4, -4..-1)
  end

  test "reverse/1" do
    assert Range.reverse(1..5) == 5..1
    assert Range.reverse(5..5) == 5..5
    assert Range.reverse(-5..1) == 1..-5
  end

  test "size/1" do
    assert Range.size(1..5) == 5
    assert Range.size(5..0) == 6
    assert Range.size(-5..5) == 11
    assert Range.size(-5..-5) == 1
  end

  test "sort/2" do
    assert Range.sort(1..5) == 1..5
    assert Range.sort(5..1) == 1..5
    assert Range.sort(5..5) == 5..5
    assert Range.sort(-1..-5) == -5..-1
    assert Range.sort(-5..-1) == -5..-1
    assert Range.sort(-1..-5, :ascending) == -5..-1
    assert Range.sort(-5..-1, :ascending) == -5..-1

    assert Range.sort(1..5, :descending) == 5..1
    assert Range.sort(5..1, :descending) == 5..1
    assert Range.sort(5..5, :descending) == 5..5
    assert Range.sort(-1..-5, :descending) == -1..-5
    assert Range.sort(-5..-1, :descending) == -1..-5

    assert_raise FunctionClauseError, fn ->
      Range.sort([1, 2, 3, 4], :descending)
    end

    assert_raise FunctionClauseError, fn ->
      Range.sort(1..5, "descending")
    end

    assert_raise FunctionClauseError, fn ->
      Range.sort(1..5, :asc)
    end
  end

  test "subset?/2" do
    assert Range.subset?(1..5, 0..6)
    refute Range.subset?(0..6, 1..5)
    assert Range.subset?(5..5, 5..5)
    assert Range.subset?(5..5, 4..5)
    refute Range.subset?(5..5, 4..4)
  end

  test "union/2" do
    assert Range.union(1..5, 4..8) == 1..8
    assert Range.union(5..1, 4..8) == 1..8
    assert Range.union(1..5, 8..4) == 1..8
    assert Range.union(5..1, 8..4) == 1..8

    assert Range.union(1..4, 4..8) == 1..8
    assert Range.union(4..1, 4..8) == 1..8
    assert Range.union(1..4, 8..4) == 1..8
    assert Range.union(4..1, 8..4) == 1..8

    assert Range.union(-1..4, -3..8) == -3..8
    assert Range.union(-1..4, -3..3) == -3..4

    # same ranges
    assert Range.union(4..4, 4..4) == 4..4
    assert Range.union(4..-1, 4..-1) == -1..4
    assert Range.union(-1..4, -1..4) == -1..4
    assert Range.union(4..-1, -1..4) == -1..4
    assert Range.union(-1..4, 4..-1) == -1..4

    # contiguous ranges
    assert Range.union(2..3, 0..1) == 0..3
    assert Range.union(3..2, 0..1) == 0..3
    assert Range.union(1..1, 2..2) == 1..2

    # Not touching
    assert Range.union(1..4, 6..8) == nil
    assert Range.union(1..4, -1..-4) == nil
    assert Range.union(1..4, -4..-1) == nil
  end

  test "to_list/1" do
    assert Range.to_list(-1..3) == [-1, 0, 1, 2, 3]
    assert Range.to_list(3..-1) == [3, 2, 1, 0, -1]
    assert Range.to_list(3..3) == [3]
  end
end
