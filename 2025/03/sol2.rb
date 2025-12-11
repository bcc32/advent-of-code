$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line.chomp.chars.map(&:to_i)
  end
end

def best_joltage(bank)
  # Inductively:
  #
  # - If the last-considered element is part of the best joltage, the digits
  # preceding it must be part of the best joltage with n-1 digits up to that
  # point.

  best_so_far = Hash.new(0)     # key is num_digits_used
  best_so_far[0] = 0
  bank.each do |n|
    new_best_so_far = best_so_far.dup

    (0...12).each do |prev_digits|
      cand = 10 * best_so_far[prev_digits] + n
      if cand > new_best_so_far[prev_digits + 1]
        new_best_so_far[prev_digits + 1] = cand
      end
    end

    best_so_far = new_best_so_far
  end

  best_so_far.values.max
end

p (input.map { |bank| best_joltage(bank) }.sum)
