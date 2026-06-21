.PHONY: all clean run deps test

PYDANTIC_MAIN ?= $(HOME)/local/pydantic-main

all: clamp

clamp: clamp.lisp clamp-internals.lisp clamp_compiler.py clamp-builtins.lisp
	./clamp.lisp

clean:
	-rm clamp

run: clamp
	rlwrap ./clamp

deps: requirements.txt
	pip install -r requirements.txt

test:
	PYDANTIC_MAIN="$(PYDANTIC_MAIN)" pytest test/
