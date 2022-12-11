$stdout = File.open('aoc.out', 'w')

monkey_items = [
  [53, 89, 62, 57, 74, 51, 83, 97],
  [85, 94, 97, 92, 56],
  [86, 82, 82],
  [94, 68],
  [83, 62, 74, 58, 96, 68, 85],
  [50, 68, 95, 82],
  [75],
  [92, 52, 85, 89, 68, 82],
]

inspected_count = [0] * 8

20.times do
  until monkey_items[0].empty?
    inspected_count[0] += 1
    item = monkey_items[0].shift
    item *= 3
    item /= 3
    if item % 13 == 0
      monkey_items[1] << item
    else
      monkey_items[5] << item
    end
  end
  until monkey_items[1].empty?
    inspected_count[1] += 1
    item = monkey_items[1].shift
    item += 2
    item /= 3
    if item % 19 == 0
      monkey_items[5] << item
    else
      monkey_items[2] << item
    end
  end
  until monkey_items[2].empty?
    inspected_count[2] += 1
    item = monkey_items[2].shift
    item += 1
    item /= 3
    if item % 11 == 0
      monkey_items[3] << item
    else
      monkey_items[4] << item
    end
  end
  until monkey_items[3].empty?
    inspected_count[3] += 1
    item = monkey_items[3].shift
    item += 5
    item /= 3
    if item % 17 == 0
      monkey_items[7] << item
    else
      monkey_items[6] << item
    end
  end
  until monkey_items[4].empty?
    inspected_count[4] += 1
    item = monkey_items[4].shift
    item += 4
    item /= 3
    if item % 3 == 0
      monkey_items[3] << item
    else
      monkey_items[6] << item
    end
  end
  until monkey_items[5].empty?
    inspected_count[5] += 1
    item = monkey_items[5].shift
    item += 8
    item /= 3
    if item % 7 == 0
      monkey_items[2] << item
    else
      monkey_items[4] << item
    end
  end
  until monkey_items[6].empty?
    inspected_count[6] += 1
    item = monkey_items[6].shift
    item *= 7
    item /= 3
    if item % 5 == 0
      monkey_items[7] << item
    else
      monkey_items[0] << item
    end
  end
  until monkey_items[7].empty?
    inspected_count[7] += 1
    item = monkey_items[7].shift
    item *= item
    item /= 3
    if item % 2 == 0
      monkey_items[0] << item
    else
      monkey_items[1] << item
    end
  end
end

p inspected_count.max(2).reduce(&:*)
