REBAR ?= rebar3
ERL ?= erl

top_dir = $(shell git rev-parse --show-toplevel)
export top_dir

ERL_TEST_FLAGS ?= "-config $(top_dir)/test/sys.config"

all: compile

compile:
	$(REBAR) compile

run: compile
	$(ERL) -pa _build/default/lib/*/ebin -s emdb -config config/sys.config

dialyzer:
	$(REBAR) dialyzer

test:
	ERL_FLAGS=$(ERL_TEST_FLAGS) $(REBAR) eunit -v -c

cover:
	$(REBAR) cover -v

clean:
	rm -rf _build priv rebar.lock

.PHONY: all compile clean test run dialyzer cover
