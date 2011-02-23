REBAR=./rebar

all:
	@$(REBAR) get-deps compile

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

