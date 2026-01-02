#!/bin/sh

[ -f quicklisp.lisp ] || wget https://beta.quicklisp.org/quicklisp.lisp
[ -f quicklisp.lisp.asc ] || wget https://beta.quicklisp.org/quicklisp.lisp.asc
gpg --verify quicklisp.lisp.asc quicklisp.lisp
sbcl --load quicklisp.lisp --script install-quicklisp.lisp
