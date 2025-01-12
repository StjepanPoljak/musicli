(defvar *seqn1-notes* (list (music:make-note :freq 440 :dur 1)
			    (music:make-note :freq 550 :dur 1)))

(defvar *seqn1* (music:make-seqn :name "test-seqn"
				 :notes *seqn1-notes*))

(defvar *track1* (music:make-track :instr "guitar"
				   :seqns (list *seqn1*)))

(defvar *my-song* (music:make-song :tempo 120
				   :tracks (list *track1*)))

(jack:with-jack-midi ()
  (music:init-song *my-song*)
  (music:set-curr-song *my-song*)
  (sleep 5))
