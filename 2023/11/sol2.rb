require 'set'

$stdout = File.open('aoc.out', 'w')
$input = File.open('aoc.in') do |f|
  f.readlines.map(&:chomp)
end

$rows_without_galaxies = Set.new
$input.size.times do |i|
  if $input[i].chars.all? { |x| x == '.' }
    $rows_without_galaxies << i
  end
end

$cols_without_galaxies = Set.new
$input[0].size.times do |j|
  if $input.all? { |row| row[j] == '.' }
    $cols_without_galaxies << j
  end
end

VAL = 1000000

def dist(i,j,ii,jj)
  i, ii = [i, ii].sort
  j, jj = [j, jj].sort
  (ii-i) + (jj-j) + \
  ((VAL-1) * ($rows_without_galaxies.count { |r| (i..ii).include?(r) })) + \
  ((VAL-1) * ($cols_without_galaxies.count { |c| (j..jj).include?(c) }))
end

sum = 0
$input.size.times do |i|
  $input[0].size.times do |j|
    next unless $input[i][j] == '#'
    $input.size.times do |ii|
      $input[0].size.times do |jj|
        next unless $input[ii][jj] == '#'
        sum += dist(i,j,ii,jj)
      end
    end
  end
end

p (sum/2)
