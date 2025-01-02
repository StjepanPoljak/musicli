(defpackage :sched
  (:use :cl)
  (:export :secs
	   :msecs
	   :insert-event
	   :make-event-queue
	   :make-event
	   :run-events-range
	   :event-queue-events))

(in-package :sched)

(defstruct event
  time
  (action nil)
  (data nil)
  )

(defstruct event-queue
  (events nil)
  )

(defun insert-event (&key evq event)
  (setf (event-queue-events evq) (sort (cons event (event-queue-events evq)) #'(lambda (e1 e2) (< (event-time e1) (event-time e2))))))

(defun run-events (evq)
  (run-events-range evq))

(defun run-events-real-time (evq &optional (sleep-secs 0.05))
  (let ((start-time (get-internal-real-time)))
    (loop while (event-queue-events evq) do
          (let* ((next-event (first (event-queue-events evq)))
		 (event-time (/ (- (event-time next-event)
				   (get-internal-real-time))
				internal-time-units-per-second)))
               (if (<= event-time start-time)
		   (progn
		     (funcall (event-action next-event))
		     (setf (event-queue-events evq) (rest (event-queue-events evq))))
		   (sleep sleep-secs))))))

(defun run-events-range (events &optional (time-range 0) (max-events 0))
  (let ((event-count 0)
	(event events))
    (loop while event
	  do (progn
	       (when (or (and (> time-range 0)
			      (> (event-time (car event)) time-range))
			 (and (> max-events 0)
			      (>= max-events event-count)))
		 (return))
	       (funcall (event-action (car event)) (event-data (car event)))
	       (setq event-count (+ event-count 1))
	       (setf event (cdr event))))
    event))

(defun secs (arg)
  (* 1000 (msecs arg)))

(defun msecs (arg)
  (* 1000 arg))
