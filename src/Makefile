PLUGIN = plugin
PLUGIN_DESC = "the VIBES tool"
SOURCE = $(wildcard *.ml) $(wildcard **/*.ml) $(wildcard **/*.mli)
Is = ""
PKGs = findlib.dynload,bap,bap-arm,bap_wp,bap_vibes
TAG = "warn(all)"
LIB = lib
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
# BUILD
#####################################################

.PHONY: build
build: build.lib build.plugin

.PHONY: build.lib
build.lib:
	@printf $(FMT_STR) "BUILDING LIB"
	$(MAKE) -C $(LIB) $*

.PHONY: build.plugin
build.plugin: build.lib install.lib
	@printf $(FMT_STR) "BUILDING PLUGIN"
	$(MAKE) -C $(PLUGIN) build


#####################################################
# INSTALL
#####################################################

.PHONY: install
install: install.plugin

.PHONY: install.plugin
install.plugin: install.lib
	@printf $(FMT_STR) "INSTALLING PLUGIN"
	$(MAKE) -C $(PLUGIN) $*

# lib must be installed BEFORE building plugin
.PHONY: install.lib
install.lib:
	@printf $(FMT_STR) "INSTALLING LIB"
	$(MAKE) -C $(LIB) install

.PHONY: uninstall
uninstall:
	bapbundle remove $(PLUGIN)


#####################################################
# CLEAN
#####################################################

.PHONY: clean
clean:
	@printf $(FMT_STR) "CLEANING LIB"
	$(MAKE) -C $(LIB) clean
	@printf $(FMT_STR) "CLEANING PLUGIN"
	$(MAKE) -C $(PLUGIN) clean


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
