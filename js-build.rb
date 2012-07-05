



system('mkdir -p dist/build/ceditor-client/ceditor-client-tmp')
system("hastec CEditorClient.hs --opt-all")
hastec_success = $?.success?
%w(o jsmod hi).each do |ext|
  `find | grep -x '.*\.#{ext}' | grep -v -x 'dist/.*'`.split("\n").each do |f|
    system("mkdir -p dist/build/ceditor-client/ceditor-client-tmp/#{f} 2> /dev/null")
    system("rmdir dist/build/ceditor-client/ceditor-client-tmp/#{f} 2> /dev/null")
    system("mv #{f} dist/build/ceditor-client/ceditor-client-tmp/#{f}")
  end
end
system("mv CEditorClient.js dist/build/ceditor-client.js 2> /dev/null")
raise "hastec failed!" unless hastec_success




