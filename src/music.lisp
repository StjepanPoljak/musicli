(defpackage :music
  (:use :cl)
  (:export :make-song
	   :make-seqn
	   :make-note
	   :make-track
	   :play-song))

(in-package :music)

(defmacro def-default (prefix)
  (let ((var-name (intern (concatenate 'string "*" (symbol-name prefix) "-NUM*")))
        (fn-name (intern (concatenate 'string (symbol-name prefix) "-DEFAULT"))))
    `(progn
       (defvar ,var-name 0)
       (defun ,fn-name ()
         (setq ,var-name (+ ,var-name 1))
         (format nil "~(~a~)~(~a~)" ',prefix ,var-name)))))

(def-default song)
(def-default seqn)
(def-default track)

(defstruct note
  freq
  dur
  (sus 0)
  )

(defstruct seqn
  (name (seqn-default))
  (notes nil)
  )

(defstruct track
  (name (track-default))
  (instr "piano")
  (seqns nil)
  )

(defstruct song
  (name (song-default))
  (tempo 120)
  (tracks nil)
  )

(defun nplay-cb (freq)
  (format t "~a: PLAY: ~a~%" (get-internal-real-time) freq))

(defun nstop-cb (freq)
  (format t "~a: STOP ~a~%" (get-internal-real-time) freq))

(defun process-seqn (seqn beat-dur nplay-evq)
  (let ((last_time 0))
    (loop for note in (seqn-notes seqn)
	  do (let ((freq (note-freq note))
		   (dur (note-dur note)))
	       (sched:insert-event nplay-evq last_time #'(lambda () (nplay-cb freq)))
	       (setf last_time (+ last_time (* beat-dur dur)))
	       (sched:insert-event nplay-evq last_time #'(lambda () (nstop-cb freq)))))))

(defun process-track (track beat-dur nplay-evq)
  (loop for seqn in (track-seqns track)
	do (process-seqn seqn beat-dur nplay-evq)))

(defun play-song (song)
  (let ((beat-dur (* (sched:secs 4) (/ 60 (song-tempo song))))
	(nplay-evq (sched:make-event-queue :events nil)))
    (format t "Playing ~a (beat duration: ~as)...~%" (song-name song) beat-dur)
    (loop for track in (song-tracks song)
	  do (process-track track beat-dur nplay-evq))
    (sched:run-events nplay-evq)))
