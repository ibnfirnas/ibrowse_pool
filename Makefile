REBAR := ./rebar

.PHONY: \
	all \
	clean \
	compile \
	deps \
	dialyze

all: \
	clean \
	deps \
	compile \
	dialyze

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

dialyze:
	@dialyzer ebin deps/*/ebin
