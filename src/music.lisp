(defpackage :music
  (:use :cl)
  (:export :make-song
	   :make-seqn
	   :make-note
	   :make-track
	   :init-song
	   :test-loop))

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
  (vel 127)
  )

(defconstant +note-on+ 0)
(defconstant +note-off+ 1)

(defstruct note-event
  note
  note-state
  )

(defstruct seqn
  (name (seqn-default))
  (notes nil)
  )

(defstruct track
  (name (track-default))
  (instr "piano")
  (seqns nil)
  (evq nil)
  (curr nil)
  )

(defstruct song
  (name (song-default))
  (tempo 120)
  (tracks nil)
  (beat-dur 0)
  )

(defun nplay-cb (note-event)
  (format t "~a: PLAY: ~a~%" (sched:get-real-time-in-seconds) (note-freq (note-event-note note-event))))

(defun nstop-cb (note-event)
  (format t "~a: STOP ~a~%" (sched:get-real-time-in-seconds) (note-freq (note-event-note note-event))))

(defun process-seqn (seqn beat-dur nplay-evq)
  (let ((last-time 0))
    (loop for note in (seqn-notes seqn)
	  do (let* ((dur (note-dur note))
		    (note-on-event (make-note-event :note note
						    :note-state +note-on+))
		    (note-off-event (make-note-event :note note
						     :note-state +note-off+))
		    (on-event (sched:make-event :time last-time
						:action #'nplay-cb
						:data note-on-event))
		    (off-event (sched:make-event :time last-time
						 :action #'nstop-cb
						 :data note-off-event)))
	       (sched:insert-event :evq nplay-evq
				   :event on-event)
	       (setf last-time (+ last-time (* beat-dur dur)))
	       (sched:insert-event :evq nplay-evq
				   :event off-event)))))

(defun process-track (track beat-dur)
  (loop for seqn in (track-seqns track)
	do (process-seqn seqn beat-dur (track-evq track))))

(defun init-beat-duration (song)
  (setf (song-beat-dur song) (* 4 (/ 60 (song-tempo song)))))

(defun init-track-event-queue (track)
  (setf (track-evq track) (sched:make-event-queue :events nil)))

(defun test-loop (track &optional (sleep-secs 0.05) (test-secs 5))
  (let ((start-time (sched:get-real-time-in-seconds)))
    (loop while (> test-secs (- (sched:get-real-time-in-seconds) start-time))
	  do (progn
	       (setf (track-curr track)
		     (sched:run-events-range (if (track-curr track)
						 (track-curr track)
						 (sched:event-queue-events (track-evq track)))
					     0 1))
	       (sleep sleep-secs)))))

(defun init-song (song)
  (init-beat-duration song)
  (dolist (track (song-tracks song))
    (init-track-event-queue track)
    (process-track track (song-beat-dur song))))
