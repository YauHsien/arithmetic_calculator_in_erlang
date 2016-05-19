.PHONY: compile test ct

compile:
	rebar compile

test:
	rebar eu

ct:
	rebar ct
