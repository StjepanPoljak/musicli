(defpackage :instr
  (:use :cl)
  (:export :add-out-client
	   :add-instr
	   :get-instr-channel
	   :instr-was-cached
	   :mark-instr-cached
	   :get-instr-select-args
	   :get-instr-by-name))

(in-package :instr)

(defstruct instr
  out-client
  channel
  bank
  program
  (cached nil)
  )

(defstruct out-client
  channels
  (midi-port nil)
  )

(defvar *instr-hash* (make-hash-table :test 'eq))
(defvar *out-client-hash* (make-hash-table :test 'eq))

(defun get-instr-select-args (instr)
  (list (instr-channel instr) (instr-bank instr) (instr-program instr)))

(defun get-instr-channel (instr)
  (instr-channel instr))

(defun add-out-client (name)
  (when (gethash name *out-client-hash*)
    (format t "Warning: Overwriting out-client \"~A\".~%" name))
  (let ((new-out-client (make-out-client :channels (make-array 16 :initial-element nil))))
	(setf (gethash name *out-client-hash*) new-out-client)))

(defun add-instr (name out-client-name bank program)
  (when (gethash name *instr-hash*)
    (format t "Warning: Overwriting instrument \"~A\".~%" name))
  (let* ((out-client (get-out-client-by-name out-client-name))
	 (channel (find-free-channel out-client))
	 (new-instr (make-instr :out-client out-client :channel channel :bank bank :program program)))
    (setf (gethash name *instr-hash*) new-instr)
    (setf (aref (out-client-channels out-client) channel) new-instr)))

(defun find-free-channel (out-client)
  (let ((res nil)
	(channels (out-client-channels out-client)))
    (dotimes (i (length channels))
      (when (not (aref channels i))
	(setf res i)
	(return)))
    res))

(defun get-instr-by-name (name)
  (gethash name *instr-hash*))

(defun get-out-client-by-name (name)
  (gethash name *out-client-hash*))

(defun instr-was-cached (instr)
  (instr-cached instr)) ; (aref (out-client-channels (instr-out-client instr)) (instr-channel instr))))

(defun mark-instr-cached (instr)
  (setf (instr-cached instr) t))
