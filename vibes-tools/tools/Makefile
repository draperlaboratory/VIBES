.DEFAULT_GOAL: all

all: install

.PHONY: build
build:
	dune build

.PHONY: install
install: build
	dune install

.PHONY: uninstall
uninstall:
	dune uninstall

.PHONY: clean
clean: uninstall
	dune clean
