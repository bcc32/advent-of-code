require 'set'
$stdout = File.open('aoc.out', 'w')

$flows = {}
$tunnels = Hash.new { |h, k| h[k] = [] }
$node_index = {}

File.open('aoc.in') do |f|
  f.readlines.each_with_index do |line, i|
    line =~ /Valve (..) has flow rate=(\d+); tunnels? leads? to valves? (.*)/ or fail line
    from = $1
    flow = $2.to_i
    to = $3.split(/, /)
    $tunnels[from] = to
    $flows[from] = flow
    $node_index[from] = i
  end
end

class Bitset
  def initialize
    @n = 0
  end

  def <<(x)
    n |= (1 << x)

    self
  end

  def include?(x)
    (n & (1 << x)) != 0
  end

  def hash
    n.hash
  end

  attr_accessor :n

  def ==(other)
    n == other.n
  end

  alias eql? ==
end

class State
  def initialize(node, ele_node, nodes_open)
    @node = node
    @ele_node = ele_node
    @nodes_open = nodes_open
  end

  attr_accessor :node, :ele_node, :nodes_open

  def ==(other)
    node == other.node &&
      ele_node == other.ele_node &&
      nodes_open == other.nodes_open
  end

  def hash
    31 * node.hash +
      7 * ele_node.hash +
      nodes_open.hash
  end

  def neighbors(minutes_left_after_step)
    n = []

    # I act.
    unless nodes_open.include?($node_index[node]) || $flows[node] == 0
      n << [$flows[node] * minutes_left_after_step,
            State.new(node, ele_node, nodes_open.dup << ($node_index[node]))]
    end

    $tunnels[node].each do |neighbor_node|
      n << [0, State.new(neighbor_node, ele_node, nodes_open)]
    end

    # Elephant acts
    m = []
    n.each do |added, state|
      unless state.nodes_open.include?($node_index[state.ele_node]) || $flows[state.ele_node] == 0
        m << [added + $flows[state.ele_node] * minutes_left_after_step,
              State.new(state.node, state.ele_node,
                        state.nodes_open.dup << ($node_index[state.ele_node]))]
      end

      $tunnels[state.ele_node].each do |ele_neighbor_node|
        m << [added + 0, State.new(state.node, ele_neighbor_node, nodes_open)]
      end
    end

    m
  end

  alias eql? ==
end

def dijkstra(start)
  q = Set.new([start])
  scores = { start => 0 }

  25.downto(0) do |minutes_left_after_step|
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

    q = new_q
    scores = new_scores
  end

  scores
end

max_flows = dijkstra(State.new('AA', 'AA', Bitset.new))
p max_flows.to_a.map(&:last).max
