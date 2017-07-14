CC=gcc
CFLAGS=-m32 -w -g
OUTPUT=testprog
LIB=lib/runtime.c

all:
	$(CC) $(CFLAGS) testprog.s $(LIB) -o $(OUTPUT) && \
			echo 'Output of ./testprog:' && ./testprog

clean:
	-rm *.c
	-rm testprog
