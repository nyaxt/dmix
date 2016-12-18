#!/usr/bin/ruby
require 'rubygems'
wd = Dir.pwd
Dir.chdir File.dirname(__FILE__)
require 'bundler/setup'
Dir.chdir wd

require 'curses'
C = Curses

statedumps = []
statedump_buf = []
in_statedump = false
ARGF.each_line do |line|
  case line
  when /<statedump>/
    in_statedump = true
  when /<\/statedump>/
    in_statedump = false
    statedumps << statedump_buf
    statedump_buf = []
  else
    statedump_buf << line if in_statedump
  end
end

if statedumps.empty?
  puts "couldn't find any statedumps"
end

TOKENIZE_REGEX = /[^\s\->,.]*[\s\->,.]*/
def print_with_diff(curr, prev)
  curr ||= ''
  prev ||= ''

  ca = curr.scan(TOKENIZE_REGEX)
  pa = prev.scan(TOKENIZE_REGEX)
  
  ca.zip(pa).each do |ct, pt|
    attr = ct == pt ? 0 : C::color_pair(C::COLOR_RED)|C::A_BOLD
    C::attron(attr) do
      C::addstr ct
    end
  end
end

C::init_screen
C::start_color
# C::use_default_colors
C::init_pair(C::COLOR_RED, C::COLOR_RED, C::COLOR_BLACK)
begin
  C::crmode
  stateidx = 0

  loop do
    C::clear
    statedump = statedumps[stateidx]
    prev_statedump = stateidx > 0 ? statedumps[stateidx - 1] : []
    C::setpos(0, 0)
    C::attron(C::A_REVERSE) do
      C::addstr "Dump Pager state[#{stateidx}]"
    end
    statedump.each_with_index do |line, i|
      C::setpos(i + 1, 0)
      prev_line = prev_statedump[i]
      print_with_diff(line, prev_line)
    end
    C::refresh 
    c = C::getch
    case c
    when 'j'
      stateidx += 1
      stateidx = [stateidx, statedumps.size - 1].min
    when 'k'
      stateidx -= 1
      stateidx = [stateidx, 0].max
    when 'q'
      break
    end
  end

  C::close_screen
rescue
  C::close_screen
  raise
end
