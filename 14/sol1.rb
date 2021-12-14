mol, rules = File.open('input') do |f|
  mol = f.readline.chomp

  f.readline

  rules = f.readlines.map do |line|
    line.chomp.split(/ -> /)
  end

  [ mol, rules ]
end

rules = rules.to_h
 mol

10.times do
  mol = (mol.chars.each_cons(2).map do |l, r|
           if rhs = rules[[l, r].join]
             l + rhs
           else l
           end
         end + [mol.chars.last]).join
end

c = mol.chars.group_by(&:itself)
a, b = c.values.minmax_by(&:size).map(&:size)
p (b - a)
