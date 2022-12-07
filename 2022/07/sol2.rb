$stdout = File.open('aoc.out', 'w')

sizes = {}
cur_dir = []
dirs_to_check = []

total_size = 0

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
    total_size += size
  else
    fail
  end
end

total_space = 70000000
unused_space = total_space - total_size
need_to_free = 30000000 - unused_space

fail unless need_to_free >= 0

dirs_can_be_freed = []

dirs_to_check.each do |dir|
  dir_size = 0
  sizes.each do |path, size|
    if path[0, dir.size] == dir
      dir_size += size
    end
  end

  if dir_size >= need_to_free
    dirs_can_be_freed << [dir, dir_size]
  end
end

p dirs_can_be_freed.map(&:last).min
