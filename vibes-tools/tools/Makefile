.DEFAULT_GOAL: all

TOOLS := \
	vibes-playground \
	vibes-parse \
	vibes-opt \
	vibes-select

###################################
# ALL
###################################

all: install

.PHONY: build
build:
	dune build $(TOOLS)

.PHONY: install
install: build
	dune install $(TOOLS)

.PHONY: uninstall
uninstall:
	dune uninstall $(TOOLS)

.PHONY: clean
clean:
	dune clean

###################################
# vibes-playground
###################################

.PHONY: playground.build
playground.build:
	dune build vibes-playground

.PHONY: playground.install
playground.install: playground.build
	dune install vibes-playground

.PHONY: playground.uninstall
playground.uninstall:
	dune uninstall vibes-playground

###################################
# vibes-parse
###################################

.PHONY: parse.build
parse.build:
	dune build vibes-parse

.PHONY: parse.install
parse.install: parse.build
	dune install vibes-parse

.PHONY: parse.uninstall
parse.uninstall:
	dune uninstall vibes-parse

###################################
# vibes-opt
###################################

.PHONY: opt.build
opt.build:
	dune build vibes-opt

.PHONY: opt.install
opt.install: opt.build
	dune install vibes-opt

.PHONY: opt.uninstall
opt.uninstall:
	dune uninstall vibes-opt

###################################
# vibes-select
###################################

.PHONY: select.build
select.build:
	dune build vibes-select

.PHONY: select.install
select.install: select.build
	dune install vibes-select

.PHONY: select.uninstall
select.uninstall:
	dune uninstall vibes-select
