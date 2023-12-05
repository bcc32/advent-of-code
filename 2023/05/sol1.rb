$stdout = File.open('aoc.out', 'w')
$seeds, $maps = File.open('aoc.in') do |f|
  paras = f.read.split("\n\n")
  seeds = paras[0].scan(/\d+/).map(&:to_i)
  maps = paras[1..-1].map do |para|
    para.split(/\n/)[1..-1].map do |line|
      dst, src, len = line.scan(/\d+/).map(&:to_i)
      [dst, src, len]
    end
  end
  [seeds, maps]
end

def follow(seed)
  $maps.each do |map|
    next_seed = seed
    map.each do |dst, src, len|
      if (src...(src+len)) === seed
        next_seed = dst+(seed-src)
        break
      end
    end
    seed = next_seed
  end
  seed
end

p ($seeds.map { |s| follow(s) }.min)
