(defpackage :sched
  (:use :cl)
  (:export :get-real-time-in-seconds
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
  (repeat-count 1)
  (time-offset 0)
  (events nil)
  )

(defun insert-event (&key evq event)
  (setf (event-queue-events evq) (sort (cons event (event-queue-events evq))
				       #'(lambda (e1 e2) (< (event-time e1)
							    (event-time e2))))))

(defun run-events (evq)
  (run-events-range evq))

(defun get-real-time-in-seconds ()
  (float (/ (get-internal-real-time) internal-time-units-per-second)))

(defun run-events-real-time (evq &optional (sleep-secs 0.05))
  (let ((start-time (get-real-time-in-seconds)))
    (loop while (event-queue-events evq) do
          (let* ((next-event (first (event-queue-events evq)))
		 (event-time (- (event-time next-event)
				(get-real-time-in-seconds))))
               (if (<= event-time start-time)
		   (progn
		     (funcall (event-action next-event))
		     (setf (event-queue-events evq) (rest (event-queue-events evq))))
		   (sleep sleep-secs))))))

(defun run-event-queue-range (evq event-cb time-range)
  (let* ((repeat-count (event-queue-repeat-count evq))
	 (curr-count repeat-count)
	 (next-event (event-queue-events evq)))
    (loop while (or (eq repeat-count 0)
		    (>= curr-count 1))
	  do (let* ((result (run-events-range next-event event-cb (event-queue-time-offset evq) time-range))
		    (event (car result)))
	       (if (event)
		   (setf next-event event)
		   (progn
		     (setf next-event (event-queue-events evq))
		     (setf (event-queue-time-offset evq) (cdr result))
		     (setq curr-count (- curr-count 1))))))))

(defun run-events-range (events event-cb time-offset time-range)
  (let ((event events)
	(last-time 0))
    (loop while event
	  do (progn
	       (when (> (event-time (car event))
			(- time-range time-offset))
		 (return))
	       (funcall event-cb (event-data (car event)))
	       (setf last-time (event-time (car event)))
	       (setf event (cdr event))))
    (const event last-time)))
