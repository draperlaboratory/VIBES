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

###################################
# vibes-playground
###################################

.PHONY: playground
playground: playground.install

.PHONY: playground.build
playground.build:
	$(MAKE) -C $(TOOLS_SRC) playground.build

.PHONY: playground.install
playground.install:
	$(MAKE) -C $(TOOLS_SRC) playground.install

.PHONY:
playground.uninstall:
	$(MAKE) -C $(TOOLS_SRC) playground.uninstall

###################################
# vibes-parse
###################################

.PHONY: parse
parse: parse.install

.PHONY: parse.build
parse.build:
	$(MAKE) -C $(TOOLS_SRC) parse.build

.PHONY: parse.install
parse.install:
	$(MAKE) -C $(TOOLS_SRC) parse.install

.PHONY:
parse.uninstall:
	$(MAKE) -C $(TOOLS_SRC) parse.uninstall

###################################
# vibes-opt
###################################

.PHONY: opt
opt: opt.install

.PHONY: opt.build
opt.build:
	$(MAKE) -C $(TOOLS_SRC) opt.build

.PHONY: opt.install
opt.install:
	$(MAKE) -C $(TOOLS_SRC) opt.install

.PHONY:
opt.uninstall:
	$(MAKE) -C $(TOOLS_SRC) opt.uninstall

###################################
# vibes-select
###################################

.PHONY: select
select: select.install

.PHONY: select.build
select.build:
	$(MAKE) -C $(TOOLS_SRC) select.build

.PHONY: select.install
select.install:
	$(MAKE) -C $(TOOLS_SRC) select.install

.PHONY:
select.uninstall:
	$(MAKE) -C $(TOOLS_SRC) select.uninstall

###################################
# vibes-as
###################################

.PHONY: as
as: as.install

.PHONY: as.build
as.build:
	$(MAKE) -C $(TOOLS_SRC) as.build

.PHONY: as.install
as.install:
	$(MAKE) -C $(TOOLS_SRC) as.install

.PHONY:
as.uninstall:
	$(MAKE) -C $(TOOLS_SRC) as.uninstall

###################################
# vibes-patch
###################################

.PHONY: patch
patch: patch.install

.PHONY: patch.build
patch.build:
	$(MAKE) -C $(TOOLS_SRC) patch.build

.PHONY: patch.install
patch.install:
	$(MAKE) -C $(TOOLS_SRC) patch.install

.PHONY:
patch.uninstall:
	$(MAKE) -C $(TOOLS_SRC) patch.uninstall
