input = File.open('aoc.in') do |f|
  # parse input
  lines = f.readlines.map(&:chomp).map { |line|
    line =~ /starting position: (\d+)/
    $1.to_i
  }
end

class Die
  def initialize
    @n = 0
  end

  def roll
    @n += 1
    if @n > 100
      @n = 1
    end

    @n
  end
end

def sim(player1, player2)
  player1_score = 0
  player2_score = 0
  die = Die.new

  turn = 1

  count_rolls = 0

  loop do
    rolls = []
    rolls << die.roll
    rolls << die.roll
    rolls << die.roll

    case turn
    when 1
      player1 += rolls.sum
      player1 -= 10 while player1 > 10
      player1_score += player1
    when 2
      player2 += rolls.sum
      player2 -= 10 while player2 > 10
      player2_score += player2
    end

    turn = 3 - turn

    count_rolls += rolls.size

    # p [player1, player1_score, player2, player2_score]

    if player1_score >= 1000
      return [1, player2_score, count_rolls]
    elsif player2_score >= 1000
      return [2, player1_score, count_rolls]
    end
  end
end

player1_start, player2_start = input

# p sim(player1_start, player2_start)
winner, loser_score, count_rolls = sim(player1_start, player2_start)
p loser_score * count_rolls
