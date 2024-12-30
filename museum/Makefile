all: clamp_shim

clean:
	-rm clamp_shim clam_shim.o

run: clamp_shim
	./clamp_shim

clamp_shim.o: clamp_shim.c
	gcc -c -Wall -I/usr/include/python3.12 clamp_shim.c -o clamp_shim.o

clamp_shim: clamp_shim.o
	gcc clamp_shim.o -o clamp_shim -lpython3.12
