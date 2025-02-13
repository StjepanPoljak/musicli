(defpackage :jack
  (:use :cl :cffi)
  (:export :with-jack-midi))

(in-package :jack)

(define-foreign-library libjack
    (:darwin (:or "libjack"))
    (:unix (:or "libjack.so"))
    (t (:default "libjack")))
   
(use-foreign-library libjack)

(cffi:defcenum jack-options-t
    (JackNullOption 0)
    (JackNoStartServer 1)
    (JackUseExactName 2)
    (JackServerName 4)
    (JackLoadName 8)
    (JackLoadInit 16)
    (JackSessionID 32))

(cffi:defcenum jack-status-t
    (JackFailure 1)
    (JackInvalidOption 2)
    (JackNameNotUnique 4)
    (JackServerStarted 8)
    (JackServerFailed 16)
    (JackServerError 32)
    (JackNoSuchClient 64)
    (JackLoadFailure 128)
    (JackInitFailure 256)
    (JackShmFailure 512)
    (JackVersionError 1024)
    (JackBackendError 2048)
    (JackClientZombie 4096))

(cffi:defcfun ("jack_client_open" jack-client-open) :pointer
   (client-name :string)
   (options jack-options-t)
   (status :pointer))

(cffi:defcenum jack-port-flags-t
  (JackPortIsInput 1)
  (JackPortIsOutput 2)
  (JackPortIsPhysical 4)
  (JackPortCanMonitor 8)
  (JackPortIsTerminal 16))

(cffi:defcfun ("jack_port_register" jack-port-register) :pointer
  (client :pointer)
  (port-name :string)
  (port-type :string)
  (flags :unsigned-long)
  (buffer-size :unsigned-long))

(cffi:defcfun ("jack_set_process_callback" jack-set-process-callback) :int
  (client :pointer)
  (process-callback :pointer)
  (arg :pointer))

(cffi:defcfun ("jack_activate" jack-activate) :int
  (client :pointer))

(cffi:defcfun ("jack_last_frame_time" jack-last-frame-time) :uint32
  (client :pointer))

(cffi:defcfun ("jack_get_sample_rate" jack-get-sample-rate) :uint32
  (client :pointer))

(cffi:defcfun ("jack_port_get_buffer" jack-port-get-buffer) :pointer
  (port :pointer)
  (nframes :uint32))

(cffi:defcfun ("jack_midi_clear_buffer" jack-midi-clear-buffer) :void
  (midi-port :pointer))

(cffi:defcfun ("jack_port_by_name" jack-port-by-name) :pointer
  (client :pointer)
  (port-name :string))

(cffi:defcfun ("jack_get_ports" jack-get-ports) :pointer
  (client :pointer)
  (name-regex :string)
  (type-regex :string)
  (port-flags :unsigned-long))

(cffi:defcfun ("jack_connect" jack-connect) :int
  (client :pointer)
  (source-port :string)
  (destination-port :string))

(cffi:defcfun ("jack_deactivate" jack-deactivate) :int
  (client :pointer))

(cffi:defcfun ("jack_client_close" jack-client-close) :int
   (client :pointer))

(cffi:defcfun ("jack_midi_event_write" jack-midi-event-write) :int
   (port-buffer :pointer)
   (time :uint32)
   (data :pointer)
   (data-size :size))

(cffi:defcstruct jack-cb-args
  (midi-out :pointer)
  (client :pointer)
  (sample-rate :uint32))

(defvar musicli-port-name "musicli-out")
(defvar musicli-client-name "musicli-jack")
(defvar *jack-cb-playing* nil)

(defun get-frame-zero-time (client sample-rate)
  (float (/ (jack-last-frame-time client) sample-rate)))

(defun get-frame-slot (event-time elapsed-time-total sample-rate)
  (* (- event-time elapsed-time-total) sample-rate))

(defun send-midi-message (midi-out-buffer list &optional (elapsed-time nil))
  (let* ((list-length (length list))
	 (midi-message (cffi:foreign-alloc :uint8 :count list-length))
	 (curr-pos 0))
    (loop while (< curr-pos list-length)
	  do (progn
	       (setf (cffi:mem-aref midi-message :uint8 curr-pos) (nth curr-pos list))
	       (setf curr-pos (+ curr-pos 1))))
    (when elapsed-time
      (format t "~A: ~A~%" elapsed-time list))
    (jack-midi-event-write midi-out-buffer 0 midi-message list-length)
    (cffi:foreign-free midi-message)))

(defun midi-reset-controllers (midi-out-buffer channel)
  (send-midi-message midi-out-buffer (list (logior #xB0 channel) 121 0)))

(defun midi-all-notes-off (midi-out-buffer channel)
  (send-midi-message midi-out-buffer (list (logior #xB0 channel) 123 0)))

(defun midi-select (midi-out-buffer channel bank program)
  (format t "Running MIDI select on channel: ~A bank: ~A program: ~A~%" channel bank program)
  (send-midi-message midi-out-buffer (list (logior #xB0 channel) 0 (logand (ash bank -7) #x7F)))
  (send-midi-message midi-out-buffer (list (logior #xB0 channel) 32 (logand bank #x7F)))
  (send-midi-message midi-out-buffer (list #x20 0))
  (send-midi-message midi-out-buffer (list (logior #xC0 channel) program)))

(defun event-callback (midi-out-buffer channel event-data elapsed-time)
  (let* ((note (music:get-note-from-event event-data))
	 (note-state (music:get-note-state event-data))
	 (velocity (music:get-note-event-velocity event-data)))
    (dolist (midi-num (music:get-note-midi-number note))
      (send-midi-message midi-out-buffer (list (logior note-state channel) midi-num velocity) elapsed-time))))

(defun track-callback (track elapsed-time nframes-range-time midi-out-buffer)
  (let* ((instr (music:get-track-instr track))
	 (channel (instr:get-instr-channel instr)))
    (when (not (instr:instr-was-cached instr))
      (apply #'midi-select (cons midi-out-buffer (instr:get-instr-select-args instr)))
      (instr:mark-instr-cached instr))
    (sched:run-event-queue-range (music:track-curr track)
				 #'(lambda(event-data)(event-callback midi-out-buffer channel event-data elapsed-time))
				 (+ elapsed-time nframes-range-time))))

(defun jack-cb-send-events (nframes client midi-out sample-rate)
  (setq *jack-cb-playing* t)
  (when (zerop (music:get-start-time))
      (music:set-start-time (get-frame-zero-time client sample-rate)))
    (let* ((midi-out-buffer (jack-port-get-buffer midi-out nframes))
	   (curr-time (get-frame-zero-time client sample-rate))
	   (elapsed-time (- curr-time (music:get-start-time)))
	   (nframes-range-time (float (/ nframes sample-rate)))
	   (lambda-cb (lambda(track)(track-callback track elapsed-time nframes-range-time midi-out-buffer))))
      (jack-midi-clear-buffer midi-out-buffer)
	(when (not (music:for-each-track lambda-cb))
	  (music:set-curr-song nil))))

(defun jack-cb-stop-event (midi-out nframes)
  (when *jack-cb-playing*
    (setq *jack-cb-playing* nil)
    (format t "JACK stopping...~%")
    (let ((midi-out-buffer (jack-port-get-buffer midi-out nframes))
	  (channel 0))
      (jack-midi-clear-buffer midi-out-buffer)
      (loop while (< channel 16)
	    do (progn
		 (midi-all-notes-off midi-out-buffer channel)
		 (setf channel (+ channel 1)))))
    (music:finish-cleanup)))

(cffi:defcallback jack-cb :int ((nframes :uint32)
				(args :pointer))
		  (cffi:with-foreign-slots ((client midi-out sample-rate) args (:struct jack-cb-args))
		    (music:with-musicli-lock
			(if (music:get-curr-song)
			    (jack-cb-send-events nframes client midi-out sample-rate)
			  (jack-cb-stop-event midi-out nframes))))
		  0)

(defmacro with-jack-midi (&optional (auto-connect nil) &body body)
  `(run-jack-main ,auto-connect (lambda () ,@body)))

(defun connect-to (client dest-name)
  (format t "Attempting to connect ~A to ~A...~%" musicli-port-name dest-name)
  (let ((conn (jack-connect client (format nil "~A:~A" musicli-client-name musicli-port-name) dest-name)))
    (when (not (zerop conn))
      (format t "Could not connect to: ~A (error: ~A).~%" dest-name conn))))

(defun start-jack-callbacks (client midi-out fn auto-connect)
  (cffi:with-foreign-object (args '(:pointer (:struct jack-cb-args)))
    (setf (cffi:foreign-slot-value args '(:struct jack-cb-args) 'midi-out) midi-out
	  (cffi:foreign-slot-value args '(:struct jack-cb-args) 'client) client
	  (cffi:foreign-slot-value args '(:struct jack-cb-args) 'sample-rate) (jack-get-sample-rate client))
    (if (zerop (jack-set-process-callback client (cffi:callback jack-cb) args))
	(if (zerop (jack-activate client))
	    (progn
	      (when auto-connect
		(connect-to client auto-connect))
	      (funcall fn)
	      (jack-deactivate client))
	  (format t "Could not activate JACK client.~%"))
      (progn
	(format t "Could not set process callback.~%")
	(jack-deactivate client)))))

(defun run-jack-main (auto-connect fn)
  (let* ((status (cffi:foreign-alloc :int))
	 (client (jack-client-open musicli-client-name JackNoStartServer status)))
    (if (cffi:null-pointer-p client)
	(format t "Could not open JACK.~%")
	(progn
	  (format t "Opened JACK (status: 0x~X).~%" (cffi:mem-ref status :int))
	  (let ((midi-out (jack-port-register client musicli-port-name "8 bit raw midi" JackPortIsOutput 0)))
	    (if (cffi:null-pointer-p midi-out)
		(format t "Could not open MIDI.~%")
	      (progn
		(format t "Opened MIDI.~%")
		(start-jack-callbacks client midi-out fn auto-connect))))
	  (when (not (cffi:null-pointer-p client))
	    (jack-client-close client)
	    (format t "Closed JACK.~%"))
	  (cffi:foreign-free status)))))
