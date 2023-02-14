SRC = main.c
SRC_PATCHED = main.patched.c

.PHONY: all build clean run

all: build patch

build: main

main: $(SRC)
	powerpc-linux-gnu-gcc -fno-stack-protector -o $@ $<

main.patched.reference: $(SRC_PATCHED)
	powerpc-linux-gnu-gcc -fno-stack-protector -o $@ $<

clean:
	$(MAKE) -C vibes clean
	rm -f main main.patched main.patched.ogre

patch: main main.patched main.patched.reference

main.patched:
	$(MAKE) -C vibes
	sh vibes/verify.sh

run-orig: main
	@echo "-----------------------------------------------------------------------"
	@echo ""
	@echo "  Running original program - make should say it's ignoring Error 80"
	@echo ""
	@echo "-----------------------------------------------------------------------"
	-QEMU_LD_PREFIX=/usr/powerpc-linux-gnu qemu-ppc main
	@echo ""

run-patched: patch
	@echo "-----------------------------------------------------------------------"
	@echo ""
	@echo "  Running patched program - make should say it's ignoring Error 255"
	@echo ""
	@echo "-----------------------------------------------------------------------"
	-QEMU_LD_PREFIX=/usr/powerpc-linux-gnu qemu-ppc main.patched
	@echo ""

test: run-orig run-patched
