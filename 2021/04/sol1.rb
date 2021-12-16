require 'set'
seq, boards = File.open('input') do |f|
  seq = f.readline.split(',').map(&:to_i)

  [ seq, f.read.split(/\n\n/).reject(&:empty?).map { |group|
      group.lines.map(&:chomp).reject(&:empty?).map { |row|
        row.split.map(&:to_i)
      }
    } ]
  # parse input
end


def is_winning(b, marked)
  return true if b.any? { |r|
    r.all? { |x| marked.include?(x) }
  }

  return true if b.transpose.any? { |r|
    r.all? { |x| marked.include?(x) }
  }
end

marked = Set.new

seq.each do |x|
  marked << x
  boards.any?  { |b|
    if is_winning(b, marked)
      p (b.flatten.reject{ |x| marked.include?(x) }.sum) * x
      exit
    end
  }
end
