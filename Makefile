.PHONY: clean upgrade compile test distclean

all: clean upgrade compile dialyzer test proper

upgrade:
	@./rebar3 plugins upgrade --all
	@./rebar3 upgrade --all

compile:
	@ERL_FLAGS="-enable-feature maybe_expr" ./rebar3 xref

lint:
	@ERL_FLAGS="-enable-feature maybe_expr" ./rebar3 lint

clean:
	@./rebar3 clean

test:
	@ERL_FLAGS="-enable-feature maybe_expr" ./rebar3 as test eunit
	@ERL_FLAGS="-enable-feature maybe_expr" ./rebar3 as test cover

proper:
	@ERL_FLAGS="-enable-feature maybe_expr" ./rebar3 as test proper

dialyzer:
	@ERL_FLAGS="-enable-feature maybe_expr" ./rebar3 dialyzer

distclean:
	@./rebar3 clean --all

publish:
	@./rebar3 hex publish

github: lint compile dialyzer test proper
