require 'set'

$stdout = File.open('aoc.out', 'w')
edges = File.open('aoc.in') do |f|
  f.readlines.flat_map do |line|
    line =~ /(\w+): (.*)/
    from = $1
    to = $2.split
    to.map { |t| [from, t] }
  end
end

edges.each(&:sort!)
edges = edges.to_set

nodes = edges.map(&:first) | edges.map(&:last)

# determined by looking at neato output
edges.delete(%w[cfn jkn])
edges.delete(%w[gst rph])
edges.delete(%w[ljm sfd])

def bfs(edges, start)
  q = [start]
  v = Set.new
  v << start

  until q.empty?
    x = q.shift
    edges.select { |e| e.include?(x) }.each do |e|
      y = (e - [x])[0]
      unless v.include?(y)
        v << y
        q << y
      end
    end
  end

  v.size
end

p bfs(edges, 'cfn') * bfs(edges, 'jkn')
