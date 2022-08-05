TOOLS_SRC := tools

###################################
# DEFAULT
###################################

.DEFAULT_GOAL := all


###################################
# ALL
###################################

all:
	$(MAKE) tools.all

.PHONY: build
build:
	$(MAKE) tools.build

.PHONY: install
install:
	$(MAKE) tools.install

.PHONY: uninstall
uninstall:
	$(MAKE) tools.uninstall

.PHONY: clean
clean:
	$(MAKE) tools.clean

###################################
# TOOLS
###################################

.PHONY: tools.all
tools.all:
	$(MAKE) tools.install

.PHONY: tools.build
tools.build:
	$(MAKE) -C $(TOOLS_SRC) build

.PHONY: tools.install
tools.install:
	$(MAKE) -C $(TOOLS_SRC) install

.PHONY:
tools.uninstall:
	$(MAKE) -C $(TOOLS_SRC) uninstall

.PHONY: tools.clean
tools.clean:
	$(MAKE) -C $(TOOLS_SRC) clean
