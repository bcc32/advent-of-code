fish = File.open('input') do |f|
  f.read.split(/,/).map(&:to_i)
end

fish = fish.group_by(&:itself).transform_values(&:size)

80.times do
  new_fish = Hash.new(0)
  fish.to_a.map { |k, v|
    if k == 0
      new_fish[6] += v
      new_fish[8] += v
    else
      new_fish[k - 1] += v
    end
  }

  fish = new_fish
  p fish
  p fish.values.sum
end
