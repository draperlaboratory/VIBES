SRC = main.c

.PHONY: all build clean run

all: build patch

build: main

main: $(SRC)
	arm-linux-gnueabihf-gcc -marm -o $@ $<

clean:
	rm -f main main.patched

patch: main main.patched

main.patched:
	bap vibes main \
    --patch=ret-3 \
    --patch-point=0x4ec \
    --patch-size=8 \
    --property="(true)" \
    -o main.patched \
    --verbose
	chmod +x main.patched

run-orig: main
	@echo "-----------------------------------------------------------------------"
	@echo ""
	@echo "  Running original program - make should say it's ignoring Error 5"
	@echo ""
	@echo "-----------------------------------------------------------------------"
	-QEMU_LD_PREFIX=/usr/arm-linux-gnueabihf qemu-arm main
	@echo ""

run-patched: patch
	@echo "-----------------------------------------------------------------------"
	@echo ""
	@echo "  Running patched program - make should say it's ignoring Error 3"
	@echo ""
	@echo "-----------------------------------------------------------------------"
	-QEMU_LD_PREFIX=/usr/arm-linux-gnueabihf qemu-arm main.patched
	@echo ""

test: run-orig run-patched
