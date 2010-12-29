task :default => [:make_erl]

task :clean do
  print "Cleaning..."
  sh "rebar clean"
  print " done\n"
end

task :make_erl do
  print "Compiling Erlang sources..."
  sh "rebar get-deps && rebar compile"
  print " done\n"
end

task :build_plt do
  sh 'dialyzer ebin --build_plt --apps erts kernel stdlib inets crypto eunit --output_plt ~/.riak_err_dialyzer_plt'
end

task :analyze do
  sh 'dialyzer --plt ~/.riak_err_dialyzer_plt -Wunmatched_returns -Werror_handling -Wrace_conditions -Wbehaviours ebin | grep --color -e "^[^:]*:\|^[^:]*$"'
end

task :updatedeps do
  print "Updating Erlang dependencies..."
  sh "rebar delete-deps && rebar get-deps"
  print " done\n"
end

task :doc do
  sh "cp doc/overview.edoc doc/html"
  sh 'erl -noshell -eval "edoc:files(filelib:wildcard(\"src/*.erl\"), [{dir, \"doc/html\"}, {includes, [\"include\"]}, {source_path, [\"include\", \"src\"]}])" -s init stop'
  sh "cd doc/html && git add . && git commit -m 'New doc version' && git push"
end

task :xref do
  sh "rebar xref"
end

task :test do
  sh "rebar eunit"
end