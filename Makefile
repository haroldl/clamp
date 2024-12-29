all: clamp clamp_shim

clamp: clamp.lisp clamp_compiler.py
	./clamp.lisp

clean:
	-rm clamp

run: clamp
	./clamp

clamp_shim.o: clamp_shim.c
	gcc -c -Wall -I/usr/include/python3.12 clamp_shim.c -o clamp_shim.o

clamp_shim: clamp_shim.o
	gcc clamp_shim.o -o clamp_shim -lpython3.12
