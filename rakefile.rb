
require 'rubygems'
require 'facter'
Facter.loadfacts

def num_cpus
  (Facter.value('processorcount') ||
   Facter.value('sp_number_processors') ||
   "1").to_i
end

task :clean_hpc do
  system('mkdir -p dist/hpc')
  system('rm dist/hpc/* 2> /dev/null')
  system('rm server-tests.tix 2> /dev/null')
  system('rm -r .hpc 2> /dev/null')
end

task :js_build do
  # TODO: figure out why this errors with:
  # /home/carl/.cabal/share/haste-compiler-0.1/stdlib.js: hGetContents: invalid argument (invalid byte sequence)
  # In the mean time, use `ruby js-build.rb`
end

task :clean => [:clean_hpc] do
  system('rm -r dist')
end

task :cabal_build do
  system('cabal configure')
  raise "cabal configure failed!" unless $?.success?
  system('cabal build')
  raise "cabal build failed!" unless $?.success?
end

task :build => [:cabal_build]

task :hlint do
  system('mkdir -p dist')
  system('hlint ServerTests.hs CEditorServer.hs CEditor --report=dist/hlint.html')
end

task :test => [:clean_hpc, :build] do
  system('mkdir -p dist/hpc')
  system("./dist/build/server-tests/server-tests +RTS -N#{num_cpus}")
  raise "tests failed!" unless $?.success?
  system('hpc report server-tests')
  system('hpc report server-tests > dist/hpc/report.txt')
  system('hpc markup server-tests --destdir=dist/hpc >> /dev/null')
  puts('Coverage data stored in dist/hpc')
  puts('Point your browser at file://' + `pwd`.strip + '/dist/hpc/hpc_index.html')
end

task :server => [:build] do
  system('./dist/build/ceditor-server/ceditor-server')
end
