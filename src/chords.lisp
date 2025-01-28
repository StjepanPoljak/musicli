(defpackage :chords
  (:use :cl)
  (:export :create))

(in-package :chords)

(defvar *chord-names* '(maj min dim aug maj7 min7 minmaj7))

(defun get-formula (chord-name)
  (case chord-name
    (maj '(0 4 7))
    (min '(0 3 7))
    (dim '(0 3 6))
    (otherwise nil)))

(defun invert (chord-midi inversion)
  (let ((inv-num (mod inversion (length chord-midi)))
	(inv chord-midi))
    (loop while (> inv-num 0)
	  do (progn
	       (setf inv-num (- inv-num 1))
	       (setf inv (append (cdr inv) (list (- (car inv) 12))))))
    inv))

(defun create (root-note chord-name &optional (inversion 0))
  (let* ((root-midi (music:get-note-sym-midi-number root-note))
	 (formula (get-formula chord-name))
	 (transp-notes (map 'list #'(lambda(n)(+ root-midi n)) formula))
	 (inverted-chord (invert transp-notes inversion)))
    (format t "~A~%" inverted-chord)
    (map 'list #'music:get-midi-note-symbol inverted-chord)))
