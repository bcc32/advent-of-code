require './common'

mol, rules = File.open('input') do |f|
  mol = f.readline.chomp

  f.readline

  rules = f.readlines.map do |line|
    line.chomp.split(/ -> /)
  end

  [ mol, rules ]
end

rules = rules.to_h

mol_pairs = CompressedString.new(mol)

40.times do
  mol_pairs.iterate(rules)
end

a, b = mol_pairs.char_count.values.minmax
p (b - a)
