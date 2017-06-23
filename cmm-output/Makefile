CC=gcc
CLFAGS=-m32
OUTPUT=testprog
LIB=lib/runtime.c

all:
	tree2c $(file) > testprog.c && $(CC) $(CFLAGS) testprog.c $(LIB) -o $(OUTPUT)

clean:
	-rm *.tree
	-rm *.c
	-rm testprog
