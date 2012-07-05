#!/usr/bin/env ruby

require 'date'
require 'thread'

$loaded_facter
def num_cpus
  require 'rubygems'
  require 'facter'
  $loaded_facter = true
  Facter.loadfacts
  (Facter.value('processorcount') ||
   Facter.value('sp_number_processors') ||
   "1").to_i
end

def clean_hpc
  system('mkdir -p dist/hpc')
  system('rm dist/hpc/* 2> /dev/null')
  system('rm server-tests.tix 2> /dev/null')
  system('rm -r .hpc 2> /dev/null')
end

def clean
  clean_hpc
  system('rm -r dist')
end

def build_cabal
  system('cabal configure')
  raise "cabal configure failed!" unless $?.success?
  system('cabal build')
  raise "cabal build failed!" unless $?.success?
end

def build_client
  system('mkdir -p dist/build/ceditor-client/ceditor-client-tmp')
  system("hastec -Wall src/Client.hs --opt-all")
  hastec_success = $?.success?
  %w(o hi jsmod).zip(%w(src src .)).each do |ext, dir|
    `find #{dir} | grep -x '.*\.#{ext}' | grep -v -x 'dist/.*'`.split("\n").each do |f|
      system("mkdir -p dist/build/ceditor-client/ceditor-client-tmp/#{f} 2> /dev/null")
      system("rmdir dist/build/ceditor-client/ceditor-client-tmp/#{f} 2> /dev/null")
      system("mv #{f} dist/build/ceditor-client/ceditor-client-tmp/#{f}")
    end
  end
  system("mv src/Client.js dist/build/ceditor-client/ceditor-client.js 2> /dev/null")
  puts "note: this won't work if facter is required!" if $loaded_facter
  raise "hastec failed!" unless hastec_success
end

def build
  tc = Thread.new { build_client }
  ts = Thread.new { build_cabal }
  tc.join
  ts.join
end

def hlint
  system('mkdir -p dist')
  system('hlint src --report=dist/hlint.html')
end

def test
  clean
  build
  system('mkdir -p dist/hpc')
  system("./dist/build/server-tests/server-tests +RTS -N#{num_cpus}")
  raise "tests failed!" unless $?.success?
  system('hpc report server-tests.tix')
  system('hpc report server-tests > dist/hpc/report.txt')
  system('hpc markup server-tests --destdir=dist/hpc >> /dev/null')
  puts('Coverage data stored in dist/hpc')
  puts('Point your browser at file://' + `pwd`.strip + '/dist/hpc/hpc_index.html')
end

def server
  system('./dist/build/ceditor-server/ceditor-server')
end

start = DateTime.now
runcount = 0
ARGV.each do |x|
  self.send(x.to_sym)
  runcount += 1
end
if runcount <= 0
  puts "Usage:   ./maker.rb [action1] [[action2]...]"
  puts "Example: ./maker.rb test"
else
  puts("started: #{start.to_s} finished: #{DateTime.now.to_s}")
end

