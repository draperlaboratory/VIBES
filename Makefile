LIB_DIR := bap-vibes
PLUGIN_DIR := plugin

VIBES_XDG_APP_DIR := ~/.vibes
MINIZINC_MODEL := $(VIBES_XDG_APP_DIR)/model.mzn

#####################################################
# DEFAULT
#####################################################

.DEFAULT_GOAL := install

$(MINIZINC_MODEL):
	mkdir -p $(VIBES_XDG_APP_DIR)
	cp resources/minizinc/model.mzn $(MINIZINC_MODEL)

install: $(MINIZINC_MODEL) 
	$(MAKE) library.install
	$(MAKE) clean -C $(PLUGIN_DIR)
	$(MAKE) plugin.install

uninstall: 
	$(MAKE) plugin.uninstall
	$(MAKE) library.uninstall


#####################################################
# LIBRARY
#####################################################

library.install:
	$(MAKE) install -C $(LIB_DIR)

library.uninstall:
	$(MAKE) uninstall -C $(LIB_DIR)


#####################################################
# PLUGIN
#####################################################

plugin.install:
	$(MAKE) install -C $(PLUGIN_DIR)

plugin.uninstall:
	$(MAKE) uninstall -C $(PLUGIN_DIR)


#####################################################
# TEST
#####################################################

test.unit:
	$(MAKE) test.unit -C $(LIB_DIR)

test.integration:
	$(MAKE) test.integration -C $(LIB_DIR)

test.system: install
	bash bin/system-tests/run-tests.bash

test:
	$(MAKE) test.unit
	$(MAKE) test.integration
	$(MAKE) test.system
