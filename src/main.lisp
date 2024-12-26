(defvar *seqn1-notes* (list (music:make-note :freq 440 :dur 1)
			    (music:make-note :freq 550 :dur 1)))

(defvar *seqn1* (music:make-seqn :name "test-seqn"
			   :notes *seqn1-notes*))

(defvar *track1* (music:make-track :instr "guitar"
			     :seqns (list *seqn1*)))

(defvar *my-song* (music:make-song :tempo 120
			     :tracks (list *track1*)))

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
		(music:play-song *my-song*))))
	(jack:jack-client-close client)
	(format t "Closed JACK.~%")
	(cffi:foreign-free status))))


