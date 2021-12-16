input = File.open('input') do |f|
  f.lines.map do |line|
    xs = line.split
    before = xs.take(xs.size - 5)
    after = xs.drop(xs.size - 4)
    [before, after]
  end
end

DIGITS = %w[ abcefg cf acdeg acdfg bcdf abdfg abdefg acf abcdefg abcdfg ]

def find_assignment(before)
  cf = before.find { |x| x.size == 2 }
  cf.chars.permutation do |c, f|
    acf = before.find { |x| x.size == 3 }
    a = (acf.chars - cf.chars)[0]
    bcdf = before.find { |x| x.size == 4 }
    bd = bcdf.chars - cf.chars
    bd.permutation do |b, d|
      eg = ('a'..'g').to_a - [a,b,c,d,f]
      eg.permutation do |e, g|
        if before.count { |x| x.include?(e) } == 4
          assn = [a,b,c,d,e,f,g].join
          if DIGITS.map { |dig| dig.tr('abcdefg', assn).chars.sort }.sort \
             == before.map { |x| x.chars.sort }.sort
            return assn
          end
        end
      end
    end
  end
end

p (input.map do |before, after|
     assn = find_assignment(before)
     after.map do |num|
       DIGITS.index(num.tr(assn, 'abcdefg').chars.sort.join)
     end.join.to_i
   end.sum)
