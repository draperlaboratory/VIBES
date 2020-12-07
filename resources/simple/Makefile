PROG = main
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
	arm-linux-gnueabi-as -g $(SRC) -o $(OBJ)
	arm-linux-gnueabi-ld -entry=$(ENTRY) $(OBJ) -o $(PROG)


#####################################################
# CLEAN
#####################################################

.PHONY: clean
clean:
	rm -rf $(OBJ)
	rm -rf $(PROG)
