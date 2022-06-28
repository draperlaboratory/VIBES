TOOLS_SRC := tools
VIBES_PLAYGROUND := vibes-playground
VIBES_PARSE := vibes-parse
VIBES_OPT := vibes-opt


###################################
# DEFAULT
###################################

.DEFAULT_GOAL := all


###################################
# ALL
###################################

all:
	$(MAKE) $(VIBES_PLAYGROUND)
	$(MAKE) $(VIBES_PARSE)
	$(MAKE) $(VIBES_OPT)

.PHONY: clean
clean:
	$(MAKE) clean.$(VIBES_PLAYGROUND)
	$(MAKE) clean.$(VIBES_PARSE)
	$(MAKE) clean.$(VIBES_OPT)

build:
	$(MAKE) build.$(VIBES_PLAYGROUND)
	$(MAKE) build.$(VIBES_PARSE)
	$(MAKE) build.$(VIBES_OPT)

install:
	$(MAKE) install.$(VIBES_PLAYGROUND)
	$(MAKE) install.$(VIBES_PARSE)
	$(MAKE) install.$(VIBES_OPT)

uninstall:
	$(MAKE) uninstall.$(VIBES_PLAYGROUND)
	$(MAKE) uninstall.$(VIBES_PARSE)
	$(MAKE) uninstall.$(VIBES_OPT)


###################################
# vibes-playground
###################################

.PHONY: clean.$(VIBES_PLAYGROUND)
clean.$(VIBES_PLAYGROUND):
	$(MAKE) clean -C $(TOOLS_SRC)/$(VIBES_PLAYGROUND)

build.$(VIBES_PLAYGROUND):
	$(MAKE) build -C $(TOOLS_SRC)/$(VIBES_PLAYGROUND)

install.$(VIBES_PLAYGROUND):
	$(MAKE) install -C $(TOOLS_SRC)/$(VIBES_PLAYGROUND)

uninstall.$(VIBES_PLAYGROUND):
	$(MAKE) uninstall -C $(TOOLS_SRC)/$(VIBES_PLAYGROUND)

$(VIBES_PLAYGROUND):
	$(MAKE) clean.$(VIBES_PLAYGROUND)
	$(MAKE) build.$(VIBES_PLAYGROUND)
	$(MAKE) install.$(VIBES_PLAYGROUND)


###################################
# vibes-parse
###################################

.PHONY: clean.$(VIBES_PARSE)
clean.$(VIBES_PARSE):
	$(MAKE) clean -C $(TOOLS_SRC)/$(VIBES_PARSE)

build.$(VIBES_PARSE):
	$(MAKE) build -C $(TOOLS_SRC)/$(VIBES_PARSE)

install.$(VIBES_PARSE):
	$(MAKE) install -C $(TOOLS_SRC)/$(VIBES_PARSE)

uninstall.$(VIBES_PARSE):
	$(MAKE) uninstall -C $(TOOLS_SRC)/$(VIBES_PARSE)

$(VIBES_PARSE):
	$(MAKE) clean.$(VIBES_PARSE)
	$(MAKE) build.$(VIBES_PARSE)
	$(MAKE) install.$(VIBES_PARSE)


###################################
# vibes-opt
###################################

.PHONY: clean.$(VIBES_OPT)
clean.$(VIBES_OPT):
	$(MAKE) clean -C $(TOOLS_SRC)/$(VIBES_OPT)

build.$(VIBES_OPT):
	$(MAKE) build -C $(TOOLS_SRC)/$(VIBES_OPT)

install.$(VIBES_OPT):
	$(MAKE) install -C $(TOOLS_SRC)/$(VIBES_OPT)

uninstall.$(VIBES_OPT):
	$(MAKE) uninstall -C $(TOOLS_SRC)/$(VIBES_OPT)

$(VIBES_OPT):
	$(MAKE) clean.$(VIBES_OPT)
	$(MAKE) build.$(VIBES_OPT)
	$(MAKE) install.$(VIBES_OPT)

