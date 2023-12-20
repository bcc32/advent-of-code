class FlipFlop
  def initialize(outputs)
    @state = 0
    @outputs = outputs
  end
  attr_reader :outputs

  def pulse(my_name, input_name, state, queued_pulses)
    case state
    when 1
    when 0
      @state = 1 - @state
      @outputs.each do |o|
        queued_pulses << [my_name, o, @state]
      end
    end
  end
end

class Conjunction
  def initialize(outputs)
    @input_last_pulse = {}
    @outputs = outputs
  end
  attr_reader :outputs

  def pulse(my_name, input_name, state, queued_pulses)
    @input_last_pulse[input_name] = state
    if @input_last_pulse.each_value.all? { |v| v == 1 }
      @outputs.each do |o|
        queued_pulses << [my_name, o, 0]
      end
    else
      @outputs.each do |o|
        queued_pulses << [my_name, o, 1]
      end
    end
  end

  def attach_input(input_name)
    @input_last_pulse[input_name] = 0
  end
end

class Broadcast
  def initialize(outputs)
    @outputs = outputs
  end
  attr_reader :outputs

  def pulse(my_name, input_name, state, queued_pulses)
    @outputs.each do |o|
      queued_pulses << [my_name, o, state]
    end
  end
end

$stdout = File.open('aoc.out', 'w')
nodes = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line.chomp!
    if line =~ /^([&%])(\w+) -> ([\w, ]+)/
      type = $1
      name = $2
      outputs = $3.split(/, /)
      case type
      when '&'
        [name, Conjunction.new(outputs)]
      when '%'
        [name, FlipFlop.new(outputs)]
      else
        p [line, name, outputs, type]
        raise
      end
    else
      line =~ /broadcaster -> ([\w, ]+)/ or raise
      outputs = $1.split(/, /)
      ['broadcaster', Broadcast.new(outputs)]
    end
  end.to_h
end

nodes.each do |name, n|
  n.outputs.each do |m|
    if nodes[m].is_a?(Conjunction)
      nodes[m].attach_input(name)
    end
  end
end

def sim_button(nodes)
  num_low = 0
  num_high = 0
  pulses = []
  pulses << ['button', 'broadcaster', 0]
  until pulses.empty?
    p pulses.first
    input_name, my_name, state = pulses.shift
    case state
    when 0
      num_low += 1
    when 1
      num_high += 1
    end

    # p my_name
    if nodes[my_name]
      nodes[my_name].pulse(my_name, input_name, state, pulses)
    end
  end

  [num_low, num_high]
end

low = 0
high = 0
1000.times do
  l, h = sim_button(nodes)
  low += l
  high += h
end

p (low * high)
