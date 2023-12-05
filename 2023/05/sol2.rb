$stdout = File.open('aoc.out', 'w')
$seeds, $maps = File.open('aoc.in') do |f|
  paras = f.read.split("\n\n")
  seeds = paras[0].scan(/\d+/).map(&:to_i)
  seeds = seeds.each_slice(2).to_a
  maps = paras[1..-1].map do |para|
    para.split(/\n/)[1..-1].map do |line|
      dst, src, len = line.scan(/\d+/).map(&:to_i)
      [dst, src, len]
    end.sort_by { |_, src, _| src }
  end
  [seeds, maps]
end

def follow(ranges, map)
  old_ranges = ranges.dup
  new_ranges = []

  until old_ranges.empty?
    start, len_range = old_ranges.pop

    map.each do |dst, src, len_map|
      if (src...(src+len_map)) === start
        if src+len_map >= start+len_range
          # includes all of range
          new_ranges << [start+(dst-src), len_range]
        else
          # includes only part of the start
          new_ranges << [start+(dst-src), src+len_map-start]
          count_not_included = len_range-(src+len_map-start)
          old_ranges << [start+count_not_included, count_not_included]
        end
      elsif (src...(src+len_map)) === start+len_range-1
        # includes only part of the end
        new_ranges << [dst, (start+len_range-1 - src + 1)]
        count_not_included = len_range - (start+len_range-1 - src + 1)
        old_ranges << [start, count_not_included]
      elsif (start...(start+len_range)) === src
        # range includes all of mapping entry
        new_ranges << [dst, len_map]
        old_ranges << [start, (src - start)] if src > start
        old_ranges << [src + len_map, (start+len_range - (src+len_map))] if (start+len_range - (src+len_map)) > 0
      else
        # no overlap
      end
    end
  end

  new_ranges
end

$maps.each do |map|
  $seeds = follow($seeds, map)
end

p $seeds.map(&:first).min
