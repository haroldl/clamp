all: clamp

clamp: clamp.lisp clamp_compiler.py clamp-builtins.lisp
	./clamp.lisp

clean:
	-rm clamp

run: clamp
	./clamp
