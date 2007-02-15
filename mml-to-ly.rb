
$length = '8'
$tempo = 120
$line_ctr = 0


class String
  def read_duration!
    duration = $length.to_s
    m = self.match(/^\d+\.*/)
    duration = self.slice!(0..m[0].length-1) if m
    m = self.match(/^\.+/)
    duration += self.slice!(0..m[0].length-1) if m

    while self.match(/^\^/) do
      self.slice!(0)
      duration += '~'+self.read_duration!
    end
    duration
  end

  def read_special!
    command = self.slice!(0..0)
    message = "\n%% special: "
    case command
    when '@' then # pitch macro
      message += "pitch macro "+self.to_i.to_s
      self.sub!(/^\d+/, '')
    when 'v' then # volume macro
      message += "volume macro "+self.to_i.to_s
      self.sub!(/^\d+/, '')
    when '0', '1', '2', '3' # timbre select
      message += "timbre select "+command
    else
      $stderr.print $line_ctr+": unknown macro "+command+"\n"
      message += "unknown"
    end
    message+"\n"
  end
end

def note_name_in(name)
  { 'c' => 0, 'd' => 2, 'e' => 4, 'f' => 5, 'g' => 7, 'a' => 9, 'b' => 11 }[name]
end

class Channel
  def initialize
    @text = ''
    @octave = 0
    @clef = "bass_8"
    @stacatto = 8
    @previous_note = 'r'
    @loop_stack = []
  end

  def emit(text)
    @text += text
  end

  def opening(name)
    "voice"+name+" = {\n"
  end

  def closing
    "\n}\n\n"
  end

  def adjustment_to_suffix(adjustment)
    case adjustment
    when -2 then 'ff'
    when -1 then 'f'
    when 0 then ''
    when 1 then 's'
    when 2 then 'x'
    else
      $stderr.print $line_ctr.to_s+": too many accidentals: "+adjustment.to_s+"\n"
      exit 1
    end
  end

  def octave_to_suffix(octave)
    octave -= 3
    name = ''
    if octave < 0 then
      (-octave).times { name += ',' }
    else
      octave.times { name += "'" }
    end
    name
  end

  def pitch_to_note_name(note, adjustment)
    note + adjustment_to_suffix(adjustment) + octave_to_suffix(@octave)
  end

  def articulation
    ## XXX should check 8 for legato
    if @stacatto < 3 then '-|'
    elsif @stacatto < 5 then '-.'
    else ''
    end
  end

  def start_a_new_loop
    @loop_stack.push(@text)
    @text = ''
  end

  def end_loop(count)
    $stderr.print $line_ctr.to_s+": loop fail!" if @loop_stack.length <= 0
    text = ''
    count.times { text += @text }
    @text = @loop_stack.pop + text
  end

  def change_octave(x)
    @octave = x
    case x
    when 0 then clef = "bass_8"
    when 1 then clef = "bass_8"
    when 2 then clef = "bass"
    when 3 then clef = "alto"
    when 4 then clef = "treble"
    when 5 then clef = "treble"
    when 6 then clef = "treble^8"
    when 7 then clef = "treble^8"
    else clef = "treble^8"
    end
    if clef != @clef then
      @clef = clef
      emit("\n\\clef \""+@clef+"\"\n")
    end
    @octave
  end

  attr_accessor :text, :stacatto, :octave, :clef, :previous_note
end

class KickChannel < Channel
  def opening(name)
    "voice"+name+" = \\drummode {\n"
  end

  def pitch_to_note_name(note, adjustment)
    $stderr.print $line_ctr.to_s+": bad kick note: "+pitch.to_s+"\n" if note != 'c'
    'bd'
  end
end

class NoiseChannel < Channel
  def opening(name)
    "voice"+name+" = \\drummode {\n"
  end

  def pitch_to_note_name(note, adjustment)
    pitch = { 'c' => 0, 'd' => 2, 'e' => 4, 'f' => 5, 'g' => 7, 'a' => 9, 'b' => 11 }[note]
    pitch += adjustment
    [ 'cyms', 'cymc', 'cyms', 'cymr', 'hh', 'cymrb', 'cymr', 'cymch', 'cymch', 'sn', 'cymc', 'sn' ][pitch]
  end
end

def munge_some_music(channels, line)
  channel_select = line.match(/^([A-E]+)/).to_s
  line = line.sub(/^[A-E]+/, '').lstrip

  channels = channels.reject { |k,v| !channel_select.match(k) }
  inner_note_munging(channels, line)
end

def inner_note_munging(channels, line)
  ## read commands one by one.
  until line.match(/^\n*$/) do
    command = line.slice!(0..0)

    ## XXX length and tempo should be per-channel, but atm we don't
    ## bother because it doesn't happen in MI.
    case command
    when 'l' # length
      $length = line.read_duration!
    when 't' then # tempo
      $tempo = line.to_i
      $stderr.print("bad tempo "+$tempo.to_s+"!") if $tempo == 0
      line.sub!(/^\d+/,'')
      channels.each { |n,c| c.emit("\n%% tempo "+$tempo.to_s+"\n") }
    when 'v' then # volume
      volume = line.match(/^(\d+)|([+-])/).to_s
      line.sub!(/^(\d+)|([+-])/,'')
      channels.each { |n,c| c.emit("\n%% volume "+volume+"\n") }
    when 'o' then # octave
      octave = line.to_i
      line.sub!(/^\d+/,'')
      channels.each { |n,c| c.change_octave(octave) }
    when '>' # relative octave
      channels.each { |n,c| c.octave += 1 }
    when '<' # relative octave
      channels.each { |n,c| c.octave -= 1 }
    when '@' then # special command
      special = line.read_special!
      channels.each { |n,c| c.emit(special) }
    when 'M' # vibrato
      vib = line.match(/^(P\d+)|(POF)/)
      line.slice!(0..vib[0].length-1)
      channels.each { |n,c| c.emit("\n%% vibrato "+vib.to_s+"\n") }
    when 'E' # pitch envelope
      vib = line.match(/^(P\d+)|(POF)/)
      line.slice!(0..vib[0].length-1)
      channels.each { |n,c| c.emit("\n%% pitch envelope "+vib.to_s+"\n") }
    when 'D' # detune
      detune = line.to_i
      line.sub!(/^-?\d+/,'')
      channels.each { |n,c| c.emit("\n%% detune "+detune.to_s+"\n") }
    when 'q' then # stacatto
      stacatto = line.to_i
      line.sub!(/^\d+/,'')
      channels.each { |n,c| c.stacatto = stacatto }
    when 'r' then # rest
      duration = line.read_duration!
      channels.each { |n,c| c.emit("r"+duration.gsub(/~/,'~r')+" ") }
    when 'w' then # wait
      duration = line.read_duration!
      channels.each do |n,c|
        c.emit('~'+c.previous_note+duration.gsub(/~/,'~'+c.previous_note)+' ')
      end
    when 'a', 'b', 'c', 'd', 'e', 'f', 'g' then
      ##pitch = note_name_in(command)
      adjustment = 0
      accidentals = line.match(/^[\-\+]+/)
      if accidentals then
        line.slice!(0..accidentals[0].length-1)
        accidentals[0].split(//).each { |x| adjustment += (x == '+' ? 1 : -1) }
      end
      duration = line.read_duration!
      channels.each do |n,c|
        note = c.pitch_to_note_name(command, adjustment)
        articulation = c.articulation
        c.previous_note = note
        c.emit(note+duration.gsub(/~/,'~'+note)+articulation+" ")
      end
    when 'n' then # literal note value
      note = 'noise('+line.to_i.to_s+')'
      line.sub!(/^\d+,/,'')
      duration = line.read_duration!
      channels.each do |n,c|
        c.previous_note = note
        c.emit(note+duration.gsub(/~/,'~'+note)+' ')
      end
    when '{' # tuplet
      channels.each { |n,c| c.emit "\n\\times 2/3 { " }
    when '}' # end tuplet
      duration = line.read_duration!
      channels.each { |n,c| c.emit "} %{"+duration+"%}\n " }
      line.sub!(/^\d+/,'')
    when '[' # loop
      channels.each { |n,c| c.start_a_new_loop }
      line = inner_note_munging(channels, line)
    when ']' then # end of loop clutter
      channels.each { |n,c| c.end_loop(line.to_i) }
      line.sub!(/^\d+/,'')
      return line
    else
      $stderr.print $line_ctr.to_s+": unknown command: "+command+"\n"+line
    end
    line = line.lstrip
  end
end



#### MAIN

name = ARGV[0] || "ch_1.mml"
file = open name
channels = Hash.new
"ABC".split(//).each { |x| channels[x] = Channel.new }
"D".split(//).each { |x| channels[x] = NoiseChannel.new }
"E".split(//).each { |x| channels[x] = KickChannel.new }

file.each_line do |line|
  $line_ctr += 1
  line = line.sub(/;.*$/, '')
  if line.match(/^#/) then  # meta-info
    print line
  elsif line.match(/^[A-E]/) then  # music data
    munge_some_music(channels, line)
  else
    $stderr.print "yikes!" unless line.match(/^[ \t\n]*$/)
  end
end

channels.each { |n,c| print c.opening(n) + c.text + c.closing }
