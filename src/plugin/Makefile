PLUGIN = vibes.plugin
PLUGIN_DESC = "the VIBES tool"
SOURCE = $(wildcard *.ml) $(wildcard **/*.ml) $(wildcard **/*.mli)
PKGs = findlib.dynload,bap,bap-arm,bap_wp,bap_vibes
TAG = "warn(all)"
Is = ""
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