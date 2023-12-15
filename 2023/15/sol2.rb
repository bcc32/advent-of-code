$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.read.chomp.split(',')
end

def hash(s)
  v = 0
  s.each_char do |c|
    v += c.ord
    v *= 17
    v = v % 256
  end
  v
end

boxes = Array.new(256) { [] }

input.each do |step|
  step =~ /(\w+)(?:(=(\d+))|-)/

  label = $1
  if $3
    focal_length = $3.to_i
  else
    focal_length = nil
  end

  box = hash(label)

  if focal_length
    if a = boxes[box].find { |l, _| l == label }
      a[1] = focal_length
    else
      boxes[box] << [label, focal_length]
    end
  else
    boxes[box].delete_if { |l, _| l == label }
  end
end

p (boxes.each_with_index.sum do |box, box_index|
  box.each_with_index.sum do |(label, focal_length), lens_index|
    (box_index + 1) * (lens_index + 1) * focal_length
  end
end)
