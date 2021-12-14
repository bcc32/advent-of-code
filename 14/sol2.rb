mol, rules = File.open('input') do |f|
  mol = f.readline.chomp

  f.readline

  rules = f.readlines.map do |line|
    line.chomp.split(/ -> /)
  end

  [ mol, rules ]
end

rules = rules.to_h

mol_pairs = ('^' + mol + '$').chars.each_cons(2).to_a.group_by(&:itself).transform_values(&:size).transform_keys(&:join)

40.times do
  new_mol_pairs = Hash.new(0)
  mol_pairs.each do |k, v|
    l, r = k.chars
    if rhs = rules[k]
      new_mol_pairs[l + rhs] += v
      new_mol_pairs[rhs + r] += v
    else
      new_mol_pairs[l + r] += v
    end
  end

  mol_pairs = new_mol_pairs
  # p mol_pairs
end
# p mol_pairs

char_count = Hash.new(0)
mol_pairs.each do |k, v|
  k.chars.each do |c|
    char_count[c] += v
  end
end

char_count.transform_values! { |v| v / 2 }
char_count.reject! { |_, v| v < 1 }

a, b = char_count.values.minmax
p (b - a)
