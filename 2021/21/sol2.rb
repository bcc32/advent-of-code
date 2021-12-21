input = File.open('aoc.in') do |f|
  # parse input
  lines = f.readlines.map(&:chomp).map { |line|
    line =~ /starting position: (\d+)/
    $1.to_i
  }
end

def sim(player1, player2)
  player1_score = 0
  player2_score = 0

  states = { [player1, player1_score, player2, player2_score] => 1 }

  turn = 1

  count_rolls = 0

  player1_wins = 0
  player2_wins = 0

  rolls = []
  (1..3).each do |die1|
    (1..3).each do |die2|
      (1..3).each do |die3|
        rolls << die1 + die2 + die3
      end
    end
  end

  until states.empty?
    new_states = Hash.new(0)
    rolls.each do |roll|
      states.each do |state, count|
        player1, player1_score, player2, player2_score = state

        case turn
        when 1
          player1 += roll
          player1 -= 10 while player1 > 10
          player1_score += player1
        when 2
          player2 += roll
          player2 -= 10 while player2 > 10
          player2_score += player2
        end

        if player1_score >= 21
          player1_wins += count
        elsif player2_score >= 21
          player2_wins += count
        else
          new_states[[player1, player1_score, player2, player2_score]] += count
        end
      end
    end

    turn = 3 - turn
    states = new_states
  end

  [player1_wins, player2_wins]
end

player1_start, player2_start = input

# p sim(player1_start, player2_start)
p1w, p2w = sim(player1_start, player2_start)
p [p1w, p2w].max
