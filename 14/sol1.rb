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
mol = CompressedString.new(mol)

10.times do
  mol.iterate(rules)
end

a, b = mol.char_count.values.minmax
p (b - a)
