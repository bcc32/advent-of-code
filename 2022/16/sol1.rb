require 'set'
$stdout = File.open('aoc.out', 'w')

$flows = {}
$tunnels = Hash.new { |h, k| h[k] = [] }
$node_list = []

File.open('aoc.in') do |f|
  f.readlines.each do |line|
    line =~ /Valve (..) has flow rate=(\d+); tunnels? leads? to valves? (.*)/ or fail line
    from = $1
    flow = $2.to_i
    to = $3.split(/, /)
    $tunnels[from] = to
    $flows[from] = flow
    $node_list << from
  end
end

class Bitset
  def initialize
    @n = 0
  end

  def <<(x)
    @n |= (1 << x)

    self
  end

  def include?(x)
    (@n & (1 << x)) != 0
  end

  def hash
    @n.hash
  end

  attr_reader :n

  def ==(other)
    @n == other.n
  end

  alias eql? ==
end

class State
  def initialize(node, nodes_open)
    @node = node
    @nodes_open = nodes_open
  end

  attr_accessor :node, :nodes_open

  def ==(other)
      node == other.node &&
      nodes_open == other.nodes_open
  end

  def hash
    31 * node.hash + nodes_open.hash
  end

  def neighbors(minutes_left_after_step)
    n = []

    # No point opening the valve with 0 flow!
    unless nodes_open.include?($node_list.index(node)) || $flows[node] == 0
      n << [$flows[node] * minutes_left_after_step,
            State.new(node, nodes_open.dup << ($node_list.index(node)))]
    end

    $tunnels[node].each do |neighbor_node|
      n << [0, State.new(neighbor_node, nodes_open)]
    end

    n
  end

  alias eql? ==
end

def dijkstra(start)
  q = Set.new([start])
  scores = { start => 0 }

  29.downto(0) do |minutes_left_after_step|
    p [minutes_left_after_step, q.size]

    new_q = Set.new
    new_scores = {}
    q.each do |state|
      score = scores[state]

      state.neighbors(minutes_left_after_step).each do |added_score, nei|
        new_q << nei
        if !new_scores[nei] || score + added_score > new_scores[nei]
          new_scores[nei] = score + added_score
        end
      end
    end

    # p new_q

    q = new_q
    scores = new_scores
  end

  scores
end

max_flows = dijkstra(State.new('AA', Bitset.new))
p max_flows.to_a.map(&:last).max
