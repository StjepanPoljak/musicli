(defpackage :sched
  (:use :cl)
  (:export :get-real-time-in-seconds
	   :insert-event
	   :make-event-queue
	   :make-event
	   :run-event-queue-range
	   :run-events-range
	   :event-queue-events))

(in-package :sched)

(defstruct event
  time
  (data nil))

(defstruct event-queue
  (repeat-count 5)
  (time-offset 0)
  (events nil)
  (curr-events nil))

(defun insert-event (&key evq event)
  (setf (event-queue-events evq) (sort (cons event (event-queue-events evq))
				       #'(lambda (e1 e2) (< (event-time e1)
							    (event-time e2))))))

(defun run-events (evq)
  (run-events-range evq))

(defun get-real-time-in-seconds ()
  (float (/ (get-internal-real-time) internal-time-units-per-second)))

(defstruct run-events-range-result
  (event nil)
  (last-time 0)
  (event-count 0))

(defun run-event-queue-range (evq event-cb time-range)
  (when (not (event-queue-curr-events evq))
    (setf (event-queue-curr-events evq) (event-queue-events evq)))
  (let* ((repeat-count (event-queue-repeat-count evq))
	 (curr-count repeat-count))
    (loop while (or (eq repeat-count -1)
		    (>= curr-count 1))
	  do (let* ((result (run-events-range (event-queue-curr-events evq) event-cb (event-queue-time-offset evq) time-range))
		    (event (run-events-range-result-event result))
		    (last-time (run-events-range-result-last-time result))
		    (event-count (run-events-range-result-event-count result)))
	       (if event
		   (if (zerop event-count)
		       (return)
		       (setf (event-queue-curr-events evq) event))
		   (progn
		     (setf (event-queue-curr-events evq) (event-queue-events evq))
		     (setf (event-queue-time-offset evq) (+ (event-queue-time-offset evq) last-time))
		     (setf curr-count (- curr-count 1))
		     (setf (event-queue-repeat-count evq) curr-count)))))))

(defun run-events-range (events event-cb time-offset time-range)
  (let ((event events)
	(last-time 0)
	(event-count 0))
    (loop while event
	  do (progn
	       (when (> (event-time (car event))
			(- time-range time-offset))
		 (return))
	       (funcall event-cb (event-data (car event)))
	       (setf last-time (event-time (car event)))
	       (setf event (cdr event))
	       (setf event-count (+ 1 event-count))))
    (make-run-events-range-result :event event
				  :last-time last-time
				  :event-count event-count)))
