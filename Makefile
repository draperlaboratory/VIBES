LIB_DIR := bap-vibes
PLUGIN_DIR := plugin

VIBES_XDG_APP_DIR := ~/.vibes
MINIZINC_MODEL_LOCAL := $(VIBES_XDG_APP_DIR)/model.mzn
MINIZINC_MODEL_SRC := ./resources/minizinc/model.mzn
MINIZINC_ISEL_MODEL_LOCAL := $(VIBES_XDG_APP_DIR)/isel_model.mzn
MINIZINC_ISEL_MODEL_SRC := ./resources/minizinc/isel_model.mzn


#####################################################
# DEFAULT
#####################################################

.DEFAULT_GOAL := install

$(MINIZINC_MODEL_LOCAL): $(MINIZINC_MODEL_SRC)
	mkdir -p $(VIBES_XDG_APP_DIR)
	cp $(MINIZINC_MODEL_SRC) $(MINIZINC_MODEL_LOCAL)
	cp $(MINIZINC_ISEL_MODEL_SRC) $(MINIZINC_ISEL_MODEL_LOCAL)

install: $(MINIZINC_MODEL_LOCAL) 
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

test.unit: clean
	$(MAKE) test.unit -C $(LIB_DIR)

test.integration:
	$(MAKE) test.integration -C $(LIB_DIR)

test.system: install
	bash bin/system-tests/run-tests.bash

test:
	$(MAKE) test.unit
	$(MAKE) test.integration
	$(MAKE) test.system

#####################################################
# CLEAN
#####################################################

clean.plugin:
	$(MAKE) clean -C $(PLUGIN_DIR)

clean.lib:
	$(MAKE) clean -C $(LIB_DIR)

clean: clean.plugin clean.lib
