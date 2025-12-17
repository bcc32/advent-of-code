$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map { |l| l.split(',').map(&:to_i) }
end

def area(a, b)
  ((a[0] - b[0]).abs + 1) * ((a[1] - b[1]).abs + 1)
end

# keyed by y coord
# list of [x, :through] or [x, :upto]
$crossings = Hash.new { |h, k| h[k] = [] }

input.each_index do |i|
  a = input[i]
  b = input[(i + 1) % input.size]
  next unless a[0] == b[0]      # only care about vertical lines
  y0, y1 = [a[1], b[1]].minmax
  ((y0+1)..(y1-1)).each do |y|
    $crossings[y] << [a[0], :through]
  end
  $crossings[y0] << [a[0], :upto, :up]
  $crossings[y1] << [a[0], :upto, :down]
end
$crossings.each_value do |crossings|
  crossings.sort!
  crossings.uniq!

  # :upto entries must come in pairs.
  #
  # If they went in the same direction, all x coords between them (incl) are
  # green but outside of that they are not green.
  #
  # If they went in different directions, we changed from inside to outside or
  # vice versa.

  # :through entries do not need to come in pairs.  They unconditionally toggle
  # insideness.
end

# return true if green at x0 and all the way through x1
def valid_row?(crossings, x0, x1)
  green = false
  state_before_pair = nil
  prev_upto_dir = nil
  crossed_x0 = false
  crossings.each do |x, kind, dir|
    if x > x0 && !crossed_x0
      return false unless green
      crossed_x0 = true
    end

    # stayed green through x1
    return true if x > x1

    case kind
    when :through
      green = !green
    when :upto
      if prev_upto_dir && prev_upto_dir != dir
        green = !state_before_pair
        prev_upto_dir = nil
        state_before_pair = nil
      elsif prev_upto_dir && prev_upto_dir == dir
        prev_upto_dir = nil
        green = state_before_pair
        state_before_pair = nil
      else
        state_before_pair = green
        green = true
        prev_upto_dir = dir
      end
    end

    # became not green between x0 and x1
    return false if crossed_x0 && !green
  end

  green
end

def valid?(a, b)
  x0, x1 = [a[0], b[0]].minmax
  y0, y1 = [a[1], b[1]].minmax
  (y0..y1).all? do |y|
    valid_row?($crossings[y], x0, x1)
  end
end

ans = input.each_with_index.flat_map do |a, i|
  (i+1...input.size).map do |j|
    b = input[j]
    valid?(a, b) ? area(a, b) : 0
  end
end.max

p ans
