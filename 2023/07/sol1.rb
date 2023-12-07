$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map(&:split)
end

def cmp_single(card_a, card_b)
  'AKQJT98765432'.index(card_b) <=> 'AKQJT98765432'.index(card_a)
end

def classify(hand)
  if hand.chars.uniq.size == 1
    [10000, hand]
  elsif hand.chars.uniq.size == 2
    a, b = hand.chars.uniq
    if hand.count(a) == 4 || hand.count(b) == 4
      [9000, hand]
    else
      # full house
      [8000, hand]
    end
  elsif hand.chars.uniq.size == 3
    a, b, c = hand.chars.uniq
    if hand.count(a) == 3 || hand.count(b) == 3 || hand.count(c) == 3
      # three of a kind
      [7000, hand]
    else
      # two pair
      [6000, hand]
    end
  elsif hand.chars.uniq.size == 4
    [ 5000, hand ]
  else
    [0, hand]
  end
end

def cmp_hands(hand_a, hand_b)
  class_a = classify(hand_a)
  class_b = classify(hand_b)
  c = class_a[0] <=> class_b[0]
  return c if c != 0

  hand_a.chars.zip(hand_b.chars).map { |a, b| cmp_single(a, b)}.find { |x| x != 0 }
end

input.sort! { |(h1, bid1), (h2, bid2)| cmp_hands(h1, h2) }

p (input.map.with_index { |(hand, bid), index| bid.to_i * (index+1)}.sum)
