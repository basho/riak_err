all:
	./rebar compile

clean:
	./rebar clean

test: all
	./rebar eunit

build_plt: all
	dialyzer ebin --build_plt --apps erts kernel stdlib inets crypto eunit --output_plt ~/.riak_err_dialyzer_plt

analyze: all
	dialyzer --plt ~/.riak_err_dialyzer_plt -Wunmatched_returns -Werror_handling -Wrace_conditions -Wbehaviours ebin | grep --color -e "^[^:]*:\|^[^:]*$$"

doc: all
	cp doc/overview.edoc doc/html
	erl -noshell -eval "edoc:files(filelib:wildcard(\"src/*.erl\"), [{dir, \"doc/html\"}, {includes, [\"include\"]}, {source_path, [\"include\", \"src\"]}])" -s init stop
	cd doc/html && git add . && git commit -m 'New doc version' && git push

xref: all
	./rebar xref