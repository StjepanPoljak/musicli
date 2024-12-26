(defpackage :sched
  (:use :cl)
  (:export :secs
	   :msecs
	   :insert-event
	   :make-event-queue
	   :run-events))

(in-package :sched)

(defstruct event
  time
  action
  )

(defstruct event-queue
  (events nil)
  )

(defun insert-event (evq time action)
  (let ((new-event (make-event :time time :action action)))
    (setf (event-queue-events evq) (sort (cons new-event (event-queue-events evq)) #'(lambda (e1 e2) (< (event-time e1) (event-time e2)))))))

(defun run-events (evq)
  (let ((start-time (get-internal-real-time)))
    (loop while (event-queue-events evq) do
          (let* ((next-event (first (event-queue-events evq)))
		 (event-time (- (event-time next-event) (get-internal-real-time))))
               (if (<= event-time start-time)
		   (progn
		     (funcall (event-action next-event))
		     (setf (event-queue-events evq) (rest (event-queue-events evq))))
		   (sleep 0.05))))))

(defun secs (arg)
  (* 1000 (msecs arg)))

(defun msecs (arg)
  (* 1000 arg))
