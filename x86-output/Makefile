CC=gcc
CFLAGS=-m32 -g
OUTPUT=testprog
LIB=lib/runtime.c

all:
	$(CC) $(CFLAGS) -o $(OUTPUT) $(file) $(LIB) && \
			echo 'Output of ./testprog:' && ./testprog

clean:
	-rm *.s
	-rm testprog
