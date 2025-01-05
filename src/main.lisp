(defvar *seqn1-notes* (list (music:make-note :freq 440 :dur 1)
			    (music:make-note :freq 550 :dur 1)))

(defvar *seqn1* (music:make-seqn :name "test-seqn"
				 :notes *seqn1-notes*))

(defvar *track1* (music:make-track :instr "guitar"
				   :seqns (list *seqn1*)))

(defvar *my-song* (music:make-song :tempo 120
				   :tracks (list *track1*)))

(cffi:defcstruct jack-cb-args
  (midi-out :pointer)
  (client :pointer))

(cffi:defcallback mycb :int ((nframes :uint32)
			     (args :pointer))
		  (cffi:with-foreign-slots ((client midi-out) args (:struct jack-cb-args))
		    (let ((midi-out-port (jack:jack-port-get-buffer midi-out nframes)))
		      (jack:jack-midi-clear-buffer midi-out-port)
		      (format t "Processing ~A frames with argument ~A.~%" nframes args)))
		  ;unsigned char note_on[3] = {0x90, 60, 100}; // 0x90 = note-on, 60 = middle C, 100 = velocity
					;jack_midi_event_write(midi_out_port, 0, note_on, sizeof(note_on));
		    0)

(defun musicli-main (client midi-out)
  (cffi:with-foreign-object (args '(:pointer (:struct jack-cb-args)))
    (setf (cffi:foreign-slot-value args '(:struct jack-cb-args) 'midi-out) midi-out
	  (cffi:foreign-slot-value args '(:struct jack-cb-args) 'client) client)
    (if (zerop (jack:jack-set-process-callback client (cffi:callback mycb) args))
	(if (zerop (jack:jack-activate client))
	    (progn
	      (music:init-song *my-song*)
	      (music:test-loop *track1*))
	      (jack:jack-deactivate client))
	    (format t "Could not activate JACK client.~%"))
	(progn
	  (format t "Could not set process callback.~%")
	  (jack:jack-deactivate client))))

(let* ((status (cffi:foreign-alloc :int))
       (client (jack:jack-client-open "musicli-jack" jack:JackNoStartServer status)))
  (if (cffi:null-pointer-p client)
      (format t "Could not open JACK.~%")
      (progn
	(format t "Opened JACK (status: 0x~X).~%" (cffi:mem-ref status :int))
	(let ((midi-out (jack:jack-port-register client "musicli-out" "8 bit raw midi" jack:JackPortIsOutput 0)))
	  (if (cffi:null-pointer-p midi-out)
	      (format t "Could not open MIDI.~%")
	      (progn
		(format t "Opened MIDI.~%")
		(musicli-main client midi-out))))
	(when (not (cffi:null-pointer-p client))
	  (jack:jack-client-close client)
	  (format t "Closed JACK.~%"))
	(cffi:foreign-free status))))
