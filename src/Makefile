PLUGIN = vibes.plugin
PLUGIN_DESC = "the VIBES tool"
SOURCE = $(wildcard *.ml) $(wildcard **/*.ml) $(wildcard **/*.mli)
Is = lib
PKGs = findlib.dynload,bap,bap-arm,bap_wp
TAG = "warn(all)"

TEST = test.native
TESTS = $(wildcard tests/*.ml)
TEST_Is = lib,tests
TEST_PKGs = $(PKGs),ounit2
TEST_TAG = $(TAG),thread


#####################################################
# DEFAULT
#####################################################

.DEFAULT_GOAL = all
all: 
	$(MAKE) uninstall 
	$(MAKE) clean 
	$(MAKE) build 
	$(MAKE) install


#####################################################
# PLUGIN
#####################################################

build: $(PLUGIN)
$(PLUGIN): $(SOURCE)
	bapbuild -use-ocamlfind \
		-pkgs $(PKGs) -Is $(Is) -tag $(TAG) $(PLUGIN)

.PHONY: install
install: build
	bapbundle update -desc $(PLUGIN_DESC) $(PLUGIN)
	bapbundle install $(PLUGIN)

.PHONY: uninstall
uninstall:
	bapbundle remove $(PLUGIN)


#####################################################
# CLEAN
#####################################################

.PHONY: clean
clean:
	bapbundle remove $(PLUGIN)
	bapbuild -clean $(PLUGIN)


#####################################################
# TESTS
#####################################################

test: build_tests run_tests

build_tests: 
	$(MAKE) build 
	$(MAKE) $(TEST)

$(TEST): $(TESTS)
	ocamlbuild -use-ocamlfind \
	       -pkgs $(TEST_PKGs) -Is $(TEST_Is) -tag $(TEST_TAG) $(TEST)

run_tests: $(TEST)
	./$(TEST)
