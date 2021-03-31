SRC = main.c
PROG_REFERENCE = main.reference
PATCH_REFERENCE = main.patched.by.vibes

.PHONY: all build clean run

all: build patch

build: main

main: $(SRC)
	arm-linux-gnueabi-gcc -marm -o $@ $<

clean:
	rm -f main main.patched $(PATCH_REFERENCE)

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
	@echo "  Running original program - make should say it's ignoring Error 5"
	@echo ""
	@echo "-----------------------------------------------------------------------"
	-QEMU_LD_PREFIX=/usr/arm-linux-gnueabi qemu-arm main
	@echo ""

run-patched: patch
	@echo "-----------------------------------------------------------------------"
	@echo ""
	@echo "  Running patched program - make should say it's ignoring Error 3"
	@echo ""
	@echo "-----------------------------------------------------------------------"
	-QEMU_LD_PREFIX=/usr/arm-linux-gnueabi qemu-arm main.patched
	@echo ""

test: run-orig run-patched

#####################################################
# PATCH THE REFERENCE EXECUTABLE
#####################################################

patch.reference: $(PROG_REFERENCE)
	bap vibes $(PROG_REFERENCE) \
                --config=config.json \
                -o $(PATCH_REFERENCE) \
                --verbose
	chmod +x $(PATCH_REFERENCE)
