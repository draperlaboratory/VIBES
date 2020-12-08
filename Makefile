SOURCE = src

.DEFAULT_GOAL = all

.PHONY: all
all: uninstall clean build install

.PHONY: build
build: 
	$(MAKE) -C $(SOURCE) $@

.PHONY: install
install:
	$(MAKE) -C $(SOURCE) $@

.PHONY: uninstall
uninstall:
	$(MAKE) -C $(SOURCE) $@

.PHONY: clean
clean:
	$(MAKE) -C $(SOURCE) $@

.PHONY: test
test:
	$(MAKE) -C $(SOURCE) $@

.PHONY: build_tests
build_tests:
	$(MAKE) -C $(SOURCE) $@

.PHONY: run_tests
run_tests:
	$(MAKE) -C $(SOURCE) $@
