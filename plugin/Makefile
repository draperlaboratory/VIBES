PLUGIN := vibes.plugin
PLUGIN_DESC := "the VIBES tool"
SRC_FILES := $(wildcard **/*.ml) $(wildcard **/*.mli)
PKGs := findlib.dynload,bap,bap-arm,bap_wp,bap-vibes,yojson
TAG := "warn(all)"
Is := "lib"

TEST_SRC_DIR := tests
TEST_EXE := $(TEST_SRC_DIR)/test.exe
TEST_SRC_FILES := $(wildcard $(TEST_SRC_DIR)/*.ml)


#####################################################
# DEFAULT
#####################################################

.DEFAULT_GOAL := all

.PHONY: all
all: install


#####################################################
# PLUGIN
#####################################################

build: $(PLUGIN)
$(PLUGIN): $(SRC_FILES)
	bapbuild -use-ocamlfind \
		-pkgs $(PKGs) -Is $(Is) -tag $(TAG) $(PLUGIN)

.PHONY: install
install: build
	bapbundle update -desc $(PLUGIN_DESC) $(PLUGIN)
	bapbundle install $(PLUGIN)

.PHONY: uninstall
uninstall:
	bapbundle remove $(PLUGIN)

.PHONY: clean
clean:
	bapbundle remove $(PLUGIN)
	bapbuild -clean $(PLUGIN)


#####################################################
# TESTS
#####################################################

test.build: build $(TEST_SRC_FILES)
	dune build $(TEST_SRC_DIR)
	@echo "" # Force a newline in terminal output

test: test.build
	dune exec $(TEST_EXE)

