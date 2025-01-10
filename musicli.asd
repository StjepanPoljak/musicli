(require :asdf)

(load (merge-pathnames "quicklisp/setup.lisp" (truename "~/")))

(quicklisp:quickload :cffi)
(quicklisp:quickload :bordeaux-threads)

(asdf:defsystem "musicli"
  :description "Music composition in Lisp"
  :author "Stjepan Poljak"
  :license "GPL"
  :version "0.1"
  :depends-on (:uiop)
  :components ((:file "src/sched")
	       (:file "src/music")
       	       (:file "src/jack")
	       (:file "src/main")))

