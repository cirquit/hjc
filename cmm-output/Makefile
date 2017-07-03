CC=gcc
CFLAGS=-m32 -w -g
OUTPUT=testprog
LIB=lib/runtime.c

all:
	tree2c $(file) > testprog.c && $(CC) $(CFLAGS) testprog.c $(LIB) -o $(OUTPUT) && \
			echo 'Output of ./testprog:' && ./testprog

clean:
	-rm *.tree
	-rm *.c
	-rm testprog
