REBAR := ./rebar

.PHONY: \
	all \
	clean \
	compile \
	deps

all: \
	clean \
	deps \
	compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile
