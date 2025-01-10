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

(cffi:defcfun ("jack_midi_event_write" jack-midi-event-write) :int
  (midi-port :pointer)
  (nframes :uint32)
  (data :pointer)
  (data-size :size))

(cffi:defcfun ("jack_deactivate" jack-deactivate) :int
  (client :pointer))

(cffi:defcfun ("jack_client_close" jack-client-close) :int
   (client :pointer))

(cffi:defcstruct jack-cb-args
  (midi-out :pointer)
  (client :pointer)
  (sample-rate :uint32))

(defun get-frame-zero-time (client sample-rate)
  (float (/ (jack-last-frame-time client) sample-rate)))

;elapsed-time * nframes = n
(cffi:defcallback jack-cb :int ((nframes :uint32)
				(args :pointer))
		  (cffi:with-foreign-slots ((client midi-out sample-rate) args (:struct jack-cb-args))
					;		    (let* ((midi-out-port (jack-port-get-buffer midi-out nframes)))
		    (when (music:get-curr-song)
		      (when (zerop (music:get-start-time))
			(music:set-start-time (get-frame-zero-time client sample-rate)))
		      (let* ((midi-out-buffer (jack-port-get-buffer midi-out nframes)))
			(jack-midi-clear-buffer midi-out-buffer))))
;		      (format t "Processing ~A frames with last frame time ~A.~%" nframes frame-zero-time)))
		  ;unsigned char note_on[3] = {0x90, 60, 100}; // 0x90 = note-on, 60 = middle C, 100 = velocity
					;jack_midi_event_write(midi_out_port, 0, note_on, sizeof(note_on));
		    0)

(defmacro with-jack-midi (&body body)
  `(run-jack-main (lambda () ,@body)))

(defun start-jack-callbacks (client midi-out fn)
  (cffi:with-foreign-object (args '(:pointer (:struct jack-cb-args)))
    (setf (cffi:foreign-slot-value args '(:struct jack-cb-args) 'midi-out) midi-out
	  (cffi:foreign-slot-value args '(:struct jack-cb-args) 'client) client
	  (cffi:foreign-slot-value args '(:struct jack-cb-args) 'sample-rate) (jack-get-sample-rate client))
    (if (zerop (jack-set-process-callback client (cffi:callback jack-cb) args))
	(if (zerop (jack-activate client))
	    (progn
	      (music:init-musicli-state)
	      (funcall fn)
	      (jack-deactivate client))
	    (format t "Could not activate JACK client.~%"))
	(progn
	  (format t "Could not set process callback.~%")
	  (jack-deactivate client)))))

(defun run-jack-main (fn)
  (let* ((status (cffi:foreign-alloc :int))
	 (client (jack-client-open "musicli-jack" JackNoStartServer status)))
    (if (cffi:null-pointer-p client)
	(format t "Could not open JACK.~%")
	(progn
	  (format t "Opened JACK (status: 0x~X).~%" (cffi:mem-ref status :int))
	  (let ((midi-out (jack-port-register client "musicli-out" "8 bit raw midi" JackPortIsOutput 0)))
	    (if (cffi:null-pointer-p midi-out)
		(format t "Could not open MIDI.~%")
		(progn
		  (format t "Opened MIDI.~%")
		  (start-jack-callbacks client midi-out fn))))
	  (when (not (cffi:null-pointer-p client))
	    (jack-client-close client)
	    (format t "Closed JACK.~%"))
	  (cffi:foreign-free status)))))
