CC=gcc
CFLAGS=-m32 -w -g
OUTPUT=testprog
LIB=lib/runtime.c

all:
	$(CC) $(CFLAGS) -o $(OUTPUT) $(FILE) $(LIB) && \
			echo 'Output of ./testprog:' && ./testprog

clean:
	-rm *.s
	-rm testprog
