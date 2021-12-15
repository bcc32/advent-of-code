class MinHeap

  def initialize(elements=nil, &block)
    @heap = []
    @cmp = block || lambda { |a, b| a <=> b }
    replace(elements) if elements
  end

 protected

  attr_reader :heap

 public

  attr_reader :cmp

  def size
    heap.size
  end

  def <<(v)
    heap << v
    heapify_up! (size - 1)
    self
  end
  alias push <<

  def shift
    return nil if empty?
    swap_at! 0, (size - 1)
    r = heap.pop
    heapify_down! 0
    r
  end

  def first
    return nil if empty?
    heap.first
  end

  def empty?
    heap.empty?
  end

  def clear
    heap.clear
    self
  end

  def replace(elements)
    heap.replace(elements.to_a)
    sort!
    self
  end

  def to_a
    a = []
    dup.each_shift { |x| a << x }
    a
  end

  # TODO: Use a more efficient impl
  def swap(v)
    r = shift
    self << v
    r
  end

  def each_shift
    until empty?
      yield shift
    end
    nil
  end

  def inspect
    "<#{self.class}: size=#{size}, first=#{first || "nil"}>"
  end

  def ==(other)
    size == other.size && to_a == other.to_a
  end

 private

  def initialize_copy(other)
    @cmp = other.cmp
    @heap = other.heap.dup
  end

  def swap_at! i1, i2
    heap[i1], heap[i2] = heap[i2], heap[i1]
  end

  def heapify_up! index
    while index > 0
      parent = (index - 1) / 2
      if cmp.call(heap[parent], heap[index]) > 0
        swap_at! parent, index
        index = parent
      else
        break
      end
    end

    self
  end

  def heapify_down! index
    loop do
      next_index = index

      if 2 * index + 1 < size && cmp.call(heap[next_index], heap[2 * index + 1]) > 0
        next_index = 2 * index + 1
      end

      if 2 * index + 2 < size && cmp.call(heap[next_index], heap[2 * index + 2]) > 0
        next_index = 2 * index + 2
      end

      break if next_index == index

      swap_at! index, next_index
      index = next_index
    end

    self
  end

  def sort!
    (size / 2 - 1).downto(0) do |i|
      heapify_down! i
    end

    self
  end

end
