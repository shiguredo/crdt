.PHONY: clean upgrade compile test distclean

all: clean upgrade compile dialyzer test proper

upgrade:
	@./rebar3 upgrade --all

compile:
	@./rebar3 xref

clean:
	@./rebar3 clean

test:
	@./rebar3 as test eunit
	@./rebar3 as test cover

proper:
	@./rebar3 as test proper

dialyzer:
	@./rebar3 dialyzer

distclean:
	@./rebar3 clean --all

publish:
	@./rebar3 hex publish
