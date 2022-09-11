PLUGIN := vibes.plugin
PLUGIN_DESC := "the VIBES tool"
SRC_FILES := $(wildcard **/*.ml) $(wildcard **/*.mli)
PKGs := findlib.dynload,bap,bap-arm,bap_wp,bap-vibes,yojson
TAG := "warn(A)"
Is := "lib"


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
