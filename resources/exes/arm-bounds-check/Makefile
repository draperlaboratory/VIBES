SRC = main.c
SRC_PATCHED = main.patched.c

.PHONY: all build clean run

all: build patch

build: main

main: $(SRC)
	arm-linux-gnueabi-gcc -marm -o $@ $< vibes_dummy.s
	strip -s $@

main.patched.reference: $(SRC_PATCHED)
	arm-linux-gnueabi-gcc -marm -o $@ $< vibes_dummy.s
	strip -s $@

clean:
	rm -f main main.patched

patch: main main.patched main.patched.reference

main.patched:
	bap vibes main \
		--config=config.json \
		-o main.patched \
		--verbose
	chmod +x main.patched

run-orig: main
	@echo "-----------------------------------------------------------------------"
	@echo ""
	@echo "  Running original program - make should say it's ignoring Error 12"
	@echo ""
	@echo "-----------------------------------------------------------------------"
	-QEMU_LD_PREFIX=/usr/arm-linux-gnueabi qemu-arm main
	@echo ""

run-patched: patch
	@echo "-----------------------------------------------------------------------"
	@echo ""
	@echo "  Running patched program - make should say it's ignoring Error 255"
	@echo ""
	@echo "-----------------------------------------------------------------------"
	-QEMU_LD_PREFIX=/usr/arm-linux-gnueabi qemu-arm main.patched
	@echo ""

test: run-orig run-patched
