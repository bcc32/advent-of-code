$stdout = File.open('aoc.out', 'w')
stacks = %w[JHGMZNTF VWJ GVLJBTH BPJNCDVL FWSMPRG GHCFBNVM DHGMR HNMVZD GNFH].map { |w| w.chars.to_a }

IO.read('aoc.in').lines[10..-1].each do |line|
  line =~ /move (\d+) from (\d+) to (\d+)/
  count, from, to = $1.to_i, $2.to_i, $3.to_i
  from -= 1
  to -= 1
  count.times do
    stacks[to] << stacks[from].pop
  end
end

puts stacks.map(&:last).join
