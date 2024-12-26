PROJECT := musicli

all: run

quicklisp:
	wget https://beta.quicklisp.org/quicklisp.lisp
	sbcl --load quicklisp.lisp --eval				\
		"(handler-case (quicklisp-quickstart:install) (error (e) (exit)))"
	rm quicklisp.lisp

run:
	sbcl --load ${PROJECT}.asd --eval "(asdf:load-system :${PROJECT})"
