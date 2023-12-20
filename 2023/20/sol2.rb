class FlipFlop
  def initialize(outputs)
    @state = 0
    @outputs = outputs
    @period = nil
  end
  attr_reader :outputs

  def pulse(my_name, input_name, state, queued_pulses, step_number)
    case state
    when 1
    when 0
      @state = 1 - @state
      @period ||= step_number
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
    @period = nil
  end
  attr_reader :outputs

  def pulse(my_name, input_name, state, queued_pulses, step_number)
    @input_last_pulse[input_name] = state
    if @input_last_pulse.each_value.all? { |v| v == 1 }
      @period ||= step_number
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

  def pulse(my_name, input_name, state, queued_pulses, step_number)
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

def sim_button(nodes, step_number)
  pulses = []
  pulses << ['button', 'broadcaster', 0]
  until pulses.empty?
    input_name, my_name, state = pulses.shift
    return true if my_name == 'rx' && state == 0

    # p my_name
    if nodes[my_name]
      nodes[my_name].pulse(my_name, input_name, state, pulses, step_number)
    end
  end
end

(1.upto(Float::INFINITY).find do |n|
   # based on printing the flip-flop state each step, it seems each flip-flop
   # is immediately periodically switching between 2^k occurrences of 0 and
   # 2^k occurrences of 1.  k may be different between each one

   unfound = nodes.each.map do |name, n|
     if !n.instance_variable_get(:@period)
       name end
   end.compact
   if unfound.sort == %w[broadcaster jm]
     p nodes.each_value.map { |n|
       (n.is_a?(Conjunction) && n.instance_variable_get(:@period)) || 1 }.reduce(&:lcm)
     break
   end
   sim_button(nodes, n)
 end)
