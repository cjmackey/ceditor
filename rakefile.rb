
task :build do
  system('cabal configure')
  raise "cabal configure failed!" unless $?.success?
  system('cabal build')
  raise "cabal build failed!" unless $?.success?
end

task :test => [:build] do
  system('./dist/build/server-tests/server-tests')
  raise "tests failed!" unless $?.success?
end

task :server => [:build] do
  system('./dist/build/ceditor-server/ceditor-server')
end
