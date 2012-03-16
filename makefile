compile: 
	rebar compile
build-plt:
	dialyzer --build_plt \
	-pa deps/webmachine/ebin \
	-pa deps/mysql/ebin \
	-pa deps/erlydtl/ebin \
	-pa deps/parse_trans/ebin \
	--output_plt mindwave_dialyzer.plt \
	--apps kernel crypto stdlib sasl inets tools xmerl erts

dialyzer: compile
	dialyzer --plt mindwave_dialyzer.plt \
		--src src \
		-pa deps/webmachine/ebin \
		-pa deps/mysql/ebin \
		-pa deps/erlydtl/ebin \
		-pa deps/parse_trans/ebin
