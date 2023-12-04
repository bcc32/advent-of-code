$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    winning, have = line.split('|')
    winning = winning.scan(/\d+/)[1..].map(&:to_i)
    have = have.scan(/\d+/).map(&:to_i)
    have_good = have.count { |x| winning.include?(x) }
    [winning, have, have_good]
  end
end

card_count = Hash.new(1)

total_cards = 0
input.each_with_index do |card, index|
  winning, have, have_good = card
  index += 1
  total_cards += card_count[index]

  have_good.times do |i|
    card_count[index + i + 1] += card_count[index]
  end
end

p total_cards
