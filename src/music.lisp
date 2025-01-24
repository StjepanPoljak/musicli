(defpackage :music
  (:use :cl :bordeaux-threads)
  (:export :make-song
	   :make-seqn
	   :make-note
	   :make-track
	   :init-song
	   :init-musicli-state
	   :set-curr-song
	   :get-curr-song
	   :set-start-time
	   :get-start-time
	   :for-each-track
	   :get-note-midi-number
	   :get-note-state
	   :get-note-from-event
	   :get-note-event-velocity
	   :finish-cleanup
	   :wait-done
	   :track-evq
	   :track-curr))

(in-package :music)

(defvar *note-names* '(C C# D D# E F F# G G# A A# B))

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
  sym
  dur
  (sus 0)
  (vel 127)
  )

(defconstant +note-on+ #x90)
(defconstant +note-off+ #x80)

(defstruct note-event
  note
  note-state
  )

(defun get-note-state (note-event)
  (note-event-note-state note-event))

(defun get-note-from-event (note-event)
  (note-event-note note-event))

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

(defstruct musicli-state
  (lock nil)
  (start-time 0)
  (cleanup 0)
  (note-hash nil)
  (curr-song nil))

(defvar *musicli-state* (make-musicli-state))

(defun generate-notes ()
  (let ((hash (make-hash-table :test 'eq)))
    (dotimes (i 256)
      (let* ((octave (floor i 12))
             (note (nth (mod i 12) *note-names*))
             (note-symbol (intern (format nil "~A~D" note octave) :music)))
        (setf (gethash note-symbol hash) i)))
    hash))

(defun init-musicli-state ()
  (setf (musicli-state-lock *musicli-state*) (bt:make-lock))
  (setf (musicli-state-note-hash *musicli-state*) (generate-notes)))

(defun get-note-midi-number (note)
  (gethash (note-sym note) (musicli-state-note-hash *musicli-state*)))

(defun get-note-velocity (note)
  (note-vel note))

(defun get-note-event-velocity (note-event)
  (if (eq (get-note-state note-event) +note-off+)
      0
    (get-note-velocity (get-note-from-event note-event))))

(defun set-curr-song (song)
  (bt:with-lock-held ((musicli-state-lock *musicli-state*))
    (setf (musicli-state-curr-song *musicli-state*) song)))

(defun get-curr-song ()
  (bt:with-lock-held ((musicli-state-lock *musicli-state*))
    (musicli-state-curr-song *musicli-state*)))

(defun finish-cleanup ()
  (set-cleanup-to 2))

(defun set-cleanup-to (state)
  (bt:with-lock-held ((musicli-state-lock *musicli-state*))
		     (setf (musicli-state-cleanup *musicli-state*) state)))

(defun wait-done ()
  (loop while (get-curr-song)
	do (sleep 0.5))
  (format t "Song done, waiting for cleanup.~%")
  (wait-cleanup))

(defun wait-cleanup ()
  (let ((local-cleanup 1))
    (loop while (<= local-cleanup 1)
	  do (bt:with-lock-held ((musicli-state-lock *musicli-state*))
	       (setf local-cleanup (musicli-state-cleanup *musicli-state*))
	       (sleep 0.5)))
    (set-cleanup-to 0)))

(defun set-start-time (time)
  (setf (musicli-state-start-time *musicli-state*) time))

(defun get-start-time ()
  (musicli-state-start-time *musicli-state*))

(defun process-seqn (seqn beat-dur nplay-evq)
  (let ((last-time 0))
    (loop for note in (seqn-notes seqn)
	  do (let* ((dur (note-dur note))
		    (note-on-event (make-note-event :note note
						    :note-state +note-on+))

		    (on-event (sched:make-event :time last-time
						:data note-on-event)))
	       (sched:insert-event :evq nplay-evq
				   :event on-event)
	       (setf last-time (+ last-time (* beat-dur dur)))
	       (let* ((note-off-event (make-note-event :note note
						      :note-state +note-off+))
		      (off-event (sched:make-event :time last-time
						   :data note-off-event)))
		 (sched:insert-event :evq nplay-evq
				     :event off-event))))))

(defun process-track (track beat-dur)
  (loop for seqn in (track-seqns track)
	do (process-seqn seqn beat-dur (track-evq track))))

(defun init-beat-duration (song)
  (setf (song-beat-dur song) (* 4 (/ 60 (song-tempo song)))))

(defun init-track-event-queue (track)
  (setf (track-evq track) (sched:make-event-queue :events nil))
  (setf (track-curr track) (track-evq track)))

(defun for-each-track (track-cb)
  (let ((tracks-done t)
	(song (get-curr-song)))
    (when song
      (dolist (track (song-tracks song))
	(progn
	  (funcall track-cb track)
	  (when (sched:event-queue-done (track-evq track))
	    (setf tracks-done nil)))))
    tracks-done))

(defun init-song (song)
  (init-beat-duration song)
  (dolist (track (song-tracks song))
    (init-track-event-queue track)
    (process-track track (song-beat-dur song))))
