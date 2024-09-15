clamp: clamp.lisp clamp_compiler.py
	./clamp.lisp

clean:
	rm clamp

run: clamp
	./clamp
