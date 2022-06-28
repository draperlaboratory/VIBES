TOOL_SRC := bin
LOCAL_BUILD_PREFIX = ../../_build/default/tools/vibes-opt
LOCALLY_BUILT_TOOL_NAME = main.exe
LOCALLY_BUILT_TOOL := $(TOOL_SRC)/$(LOCALLY_BUILT_TOOL_NAME)
LOCALLY_BUILT_TOOL_PATH := $(LOCAL_BUILD_PREFIX)/$(LOCALLY_BUILT_TOOL)

INSTALL_PREFIX := ${OPAM_SWITCH_PREFIX}/bin
INSTALLED_TOOL_NAME := vibes-opt
INSTALLED_TOOL_PATH := $(INSTALL_PREFIX)/$(INSTALLED_TOOL_NAME)


###################################
# DEFAULT
###################################

.DEFAULT_GOAL := all
all: clean build


###################################
# THE EXE/TOOL
###################################

.PHONY: clean
clean:
	dune clean

build:
	dune build ./$(LOCALLY_BUILT_TOOL)

install: build
	install $(LOCALLY_BUILT_TOOL_PATH) $(INSTALLED_TOOL_PATH)

uninstall:
	rm -rf $(INSTALLED_TOOL_PATH)
