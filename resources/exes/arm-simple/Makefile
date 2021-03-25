PROG = main
PATCH = main.patched
SRC = main.s
OBJ = main.o
PROG_REFERENCE = main.reference
PATCH_REFERENCE = main.reference.patched


#####################################################
# DEFAULT
#####################################################

.DEFAULT_GOAL = all
all: clean build


#####################################################
# BUILD
#####################################################

build: $(PROG)
$(PROG): $(SRC)
	arm-linux-gnueabi-gcc -marm $(SRC) -o $(PROG)


#####################################################
# CLEAN
#####################################################

.PHONY: clean
clean:
	rm -rf $(OBJ) $(PROG) $(PATCH)


#####################################################
# PATCH
#####################################################

$(PATCH): $(PROG)
	bap vibes $(PROG) \
		--config=config.json \
		-o $(PATCH) \
		--verbose
	chmod +x $(PATCH)

patch: $(PATCH)


#####################################################
# PATCH THE REFERENCE EXECUTABLE
#####################################################

patch.reference: $(PROG_REFERENCE)
	bap vibes $(PROG_REFERENCE) \
		--config=config.json \
		-o $(PATCH_REFERENCE) \
		--verbose
	chmod +x $(PATCH_REFERENCE)
