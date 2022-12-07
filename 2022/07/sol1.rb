$stdout = File.open('aoc.out', 'w')

sizes = {}
cur_dir = []
dirs_to_check = []

IO.read('aoc.in').lines.each do |line|
  line.chomp!
  if line =~ /^[$] cd (.*)/
    case $1
    when '/'
      cur_dir = []
    when '..'
      cur_dir.pop
    else
      cur_dir << $1
    end
  elsif line == '$ ls'
  elsif line =~ /^dir (.*)$/
    dirs_to_check << cur_dir + [$1]
  elsif line =~ /^(\d+) (.*)$/
    size = $1.to_i
    file_name = $2
    sizes[cur_dir + [file_name]] = size
  else
    fail
  end
end

sum = 0

dirs_to_check.each do |dir|
  dir_size = 0
  sizes.each do |path, size|
    if path[0, dir.size] == dir
      dir_size += size
    end
  end

  if dir_size <= 100000
    sum += dir_size
  end
end


p sum
