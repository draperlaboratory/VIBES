LIB_DIR := lib
PLUGIN_DIR := plugin


#####################################################
# DEFAULT
#####################################################

.DEFAULT_GOAL := install

install: 
	$(MAKE) library.install 
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
