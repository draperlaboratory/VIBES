PROG = main
PATCH = main.patched
SRC = main.s
OBJ = main.o
ENTRY = main


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
