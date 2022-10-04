.DEFAULT_GOAL: all

TOOLS := \
	vibes-playground \
	vibes-parse \
	vibes-opt \
	vibes-select \
	vibes-as \
	vibes-patch \
	vibes-init

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

###################################
# vibes-as
###################################

.PHONY: as.build
as.build:
	dune build vibes-as

.PHONY: as.install
as.install: as.build
	dune install vibes-as

.PHONY: as.uninstall
as.uninstall:
	dune uninstall vibes-as

###################################
# vibes-patch
###################################

.PHONY: patch.build
patch.build:
	dune build vibes-patch

.PHONY: patch.install
patch.install: patch.build
	dune install vibes-patch

.PHONY: patch.uninstall
patch.uninstall:
	dune uninstall vibes-patch

###################################
# vibes-init
###################################

.PHONY: init.build
init.build:
	dune build vibes-init

.PHONY: init.install
init.install: init.build
	dune install vibes-init

.PHONY: init.uninstall
init.uninstall:
	dune uninstall vibes-init
