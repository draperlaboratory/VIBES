SRC := main.c

PROG := main
PATCHED_PROG := main.patched

REF_PROG := main.reference
PATCHED_REF_PROG := main.patched.reference

PATCHED_PROG_FOR_TESTING := test.patched.by.vibes


#####################################################
# DEFAULT
#####################################################

.DEFAULT_GOAL := all
all: clean build patch


#####################################################
# BUILD
#####################################################

build: $(PROG)
$(PROG): $(SRC)
	arm-linux-gnueabi-gcc -marm -o $@ $<


#####################################################
# CLEAN
#####################################################

.PHONY: clean
clean:
<<<<<<< HEAD
	rm -rf $(OBJ) $(PROG) $(PATCHED_PROG) $(PATCHED_PROG_FOR_TESTING)


#####################################################
# PATCH
#####################################################

$(PATCHED_PROG): $(PROG)
	bap vibes $(PROG) \
                --config=config.json \
                -o $(PATCHED_PROG) \
                --verbose
	chmod +x $(PATCHED_PROG)

patch: $(PATCHED_PROG)


#####################################################
# PATCH THE REFERENCE EXECUTABLE
#####################################################

$(PATCHED_PROG_FOR_TESTING): $(REF_PROG)
	bap vibes $(REF_PROG) \
                --config=config.json \
                -o $(PATCHED_PROG_FOR_TESTING) \
                --verbose
	chmod +x $(PATCHED_PROG_FOR_TESTING)

patch.reference: $(PATCHED_PROG_FOR_TESTING)


#####################################################
# CREATE REFERENCE FILES
#####################################################

$(REF_PROG):
	$(MAKE) $(PROG)
	cp $(PROG) $(REF_PROG)

$(PATCHED_REF_PROG):
	$(MAKE) patch.reference
	cp $(PATCHED_PROG_FOR_TESTING) $(PATCHED_REF_PROG)

reference: 
	rm -rf $(REF_PROG) $(PATCHED_REF_PROG)
	$(MAKE) $(REF_PROG)
	$(MAKE) $(PATCHED_REF_PROG)


#####################################################
# RUN
#####################################################

.PHONY: run.orig
run.orig: $(PROG)
	-QEMU_LD_PREFIX=/usr/arm-linux-gnueabi qemu-arm $(PROG)

.PHONY: run.patched
run.patched: $(PATCHED_PROG)
	-QEMU_LD_PREFIX=/usr/arm-linux-gnueabi qemu-arm $(PATCHED_PROG)

.PHONY: run.ref
run.ref: $(REF_PROG)
	-QEMU_LD_PREFIX=/usr/arm-linux-gnueabi qemu-arm $(REF_PROG)

.PHONY: run.patched-ref
run.patched-ref: $(PATCHED_REF_PROG)
	-QEMU_LD_PREFIX=/usr/arm-linux-gnueabi qemu-arm $(PATCHED_REF_PROG)

.PHONY: run.test
run.test: run.orig run.patched
=======
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
>>>>>>> Fixing raw_ir exception
