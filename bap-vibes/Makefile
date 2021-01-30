LIB := bap-vibes
SRC_FILES := $(wildcard **/*.ml) $(wildcard **/*.mli)


#####################################################
# DEFAULT
#####################################################

.DEFAULT_GOAL := all

.PHONY: all
all: 
	$(MAKE) clean
	$(MAKE) build


#####################################################
# CLEAN
#####################################################

.PHONY: clean
clean: 
	dune clean


#####################################################
# BUILD
#####################################################

build: $(SRC_FILES)
	dune build -p $(LIB)


#####################################################
# INSTALL
#####################################################

.PHONY: install
install:
	opam install .

.PHONY: uninstall
uninstall:
	opam remove .


#####################################################
# TEST
#####################################################

test.unit: build
	dune runtest tests/unit

test.integration: build
	dune runtest tests/integration

test:
	$(MAKE) test.unit
	$(MAKE) test.integration

