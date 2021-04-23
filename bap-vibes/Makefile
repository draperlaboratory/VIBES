LIB := bap-vibes
LIB_SRC_DIR := src
LIB_SRC_FILES := \
  $(wildcard $(LIB_SRC_DIR)/*.mli) \
  $(wildcard $(LIB_SRC_DIR)/*.ml)

TEST_SRC_DIR := tests

UNIT_TEST_SRC_DIR := $(TEST_SRC_DIR)/unit
UNIT_TEST_EXE := $(UNIT_TEST_SRC_DIR)/test.exe
UNIT_TEST_SRC_FILES := \
  $(wildcard $(UNIT_TEST_SRC_DIR)/*.mli) \
  $(wildcard $(UNIT_TEST_SRC_DIR)/*.ml)

INTG_TEST_SRC_DIR := $(TEST_SRC_DIR)/integration
INTG_TEST_EXE := $(INTG_TEST_SRC_DIR)/test.exe
INTG_TEST_CONF := $(INTG_TEST_SRC_DIR)/test.conf
INTG_TEST_SRC_FILES := \
  $(wildcard $(INTG_TEST_SRC_DIR)/*.mli) \
  $(wildcard $(INTG_TEST_SRC_DIR)/*.ml)


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

build: $(LIB_SRC_FILES)
	dune build -p $(LIB) @install
	@echo "" # Force a newline in terminal output


#####################################################
# INSTALL
#####################################################

.PHONY: install
install:
	dune install

.PHONY: uninstall
uninstall:
	dune uninstall


#####################################################
# TEST
#####################################################

unit-test.build: build $(UNIT_TEST_SRC_FILES)
	dune build $(UNIT_TEST_SRC_DIR)
	@echo "" # Force a newline in terminal output

test.unit: unit-test.build
	dune exec $(UNIT_TEST_EXE)

integration-test.build: build $(INTG_TEST_SRC_FILES)
	dune build $(INTG_TEST_SRC_DIR)
	@echo "" # Force a newline in terminal output

test.integration: integration-test.build
	dune exec -- $(INTG_TEST_EXE) -conf $(INTG_TEST_CONF)

test:
	$(MAKE) test.unit
	$(MAKE) test.integration
