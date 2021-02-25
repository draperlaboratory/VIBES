SRC = main.c

.PHONY: all build clean run

all: build patch

build: main

main: $(SRC)
	arm-linux-gnueabi-gcc -mthumb -o $@ $<

clean:
	rm -f main main.patched

patch: main main.patched

main.patched:
	bap vibes main \
    --config=config.json \
    -o main.patched \
    --verbose
	chmod +x main.patched

run-orig: main
	@echo "-----------------------------------------------------------------------"
	@echo ""
	@echo "  Running original program - no error should be indicated"
	@echo ""
	@echo "-----------------------------------------------------------------------"
	-QEMU_LD_PREFIX=/usr/arm-linux-gnueabi qemu-arm main
	@echo ""

run-patched: patch
	@echo "-----------------------------------------------------------------------"
	@echo ""
	@echo "  Running patched program - make should say it's ignoring Error 1"
	@echo ""
	@echo "-----------------------------------------------------------------------"
	-QEMU_LD_PREFIX=/usr/arm-linux-gnueabi qemu-arm main.patched
	@echo ""

test: run-orig run-patched
