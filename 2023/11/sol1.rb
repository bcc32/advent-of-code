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

# def bfs(i, j)
#   q = [[i, j]]
#   dist = { [i, j] => 0 }
#   sum_dist_to_galaxies = 0
#   until q.empty?
#     i, j = q.delete_at(q.each_with_index.min_by { |(i, j), index| dist[[i,j]] }.last)
#     raise unless dist[[i,j]]

#     if $input[i][j] == '#'
#       sum_dist_to_galaxies += dist[[i, j]]
#     end

#     [[-1, 0], [1, 0], [0, 1], [0, -1]].each do |di, dj|
#       next if dist[[i+di][j+dj]]
#       if $input[i+di] && $input[i + di][j + dj]
#         if $rows_without_galaxies.include?(i+di) ||
#            $cols_without_galaxies.include?(j+dj)
#           q << [i+di,j+dj]
#           dist[[i+di,j+dj]] = dist[[i,j]] + 2
#         else
#           q << [i+di,j+dj]
#           dist[[i+di,j+dj]] = dist[[i,j]] + 1
#         end
#       end
#     end
#   end

#   sum_dist_to_galaxies
# end

def dist(i,j,ii,jj)
  i, ii = [i, ii].sort
  j, jj = [j, jj].sort
  (ii-i) + (jj-j) + \
  $rows_without_galaxies.count { |r| (i..ii).include?(r) } + \
  $cols_without_galaxies.count { |c| (j..jj).include?(c) }
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
