require 'minitest/autorun'
require_relative './min_heap'

class TestMinHeap < Minitest::Test
  def setup
    @example_data = 256.times.map { Random::rand(256) }
  end

  attr_reader :example_data

  def test_new_nonempty
    h = MinHeap.new([4, 3, 1, 2, 5])
    assert_equal [1, 2, 3, 4, 5], h.to_a
  end

  def test_push
    h = MinHeap.new
    [4, 3, 1, 2, 5].each { |x| h << x }
    assert_equal [1, 2, 3, 4, 5], h.to_a
  end

  def test_push_random
    h = MinHeap.new
    example_data.each { |x| h << x }
    assert_equal example_data.sort, h.to_a
  end

  def test_dup
    h1 = MinHeap.new(example_data)
    h2 = h1.dup
    a1 = []; h1.each_shift { |x| a1 << x }
    a2 = []; h2.each_shift { |x| a2 << x }
    assert_equal a1, a2
    assert_empty h1
    assert_empty h2
  end
end
