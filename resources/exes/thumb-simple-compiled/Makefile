SRC := main.c

PROG := main
PATCHED_PROG := main.patched
PATCHED_OGRE := main.patched.orge

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
	arm-linux-gnueabi-gcc -mthumb -o $@ $<


#####################################################
# CLEAN
#####################################################

.PHONY: clean
clean:
	$(MAKE) -C vibes clean
	rm -rf $(OBJ) $(PROG) $(PATCHED_PROG) $(PATCHED_OGRE) $(PATCHED_PROG_FOR_TESTING)


#####################################################
# PATCH
#####################################################

$(PATCHED_PROG): $(PROG)
	$(MAKE) -C vibes

patch: $(PATCHED_PROG)
	sh vibes/verify.sh


#####################################################
# PATCH THE REFERENCE EXECUTABLE
#####################################################

$(PATCHED_PROG_FOR_TESTING): $(REF_PROG)
	cp $(REF_PROG) $(PROG)
	$(MAKE) -C vibes
	cp $(PATCHED_PROG) $(PATCHED_PROG_FOR_TESTING)

patch.reference: $(PATCHED_PROG_FOR_TESTING)
	sh vibes/verify.sh


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
