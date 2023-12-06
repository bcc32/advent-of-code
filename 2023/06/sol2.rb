$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  time = f.readline.scan(/\d+/).map(&:to_i).join.to_i
  distance = f.readline.scan(/\d+/).map(&:to_i).join.to_i
  [[time,distance]]
end

def ways_to_beat(time, distance)
  (0..time).count do |my_time_spent_holding|
    speed = my_time_spent_holding
    time_coasting = time - my_time_spent_holding
    my_distance = speed * time_coasting
    my_distance > distance
  end
end

p input.map { |time,distance| ways_to_beat(time, distance) }.reduce(&:*)
