(music:init-musicli-state)

(defvar *seqn1-notes* (list (music:make-note :sym 'music::C6 :dur 1)
			    (music:make-note :sym (list 'music::B5 'music::D6) :dur 1)
			    (music:make-note :sym (chords::create 'music::A5 'chords::min) :dur 1)))

(format t "~A~%" *seqn1-notes*)

(defvar *seqn1* (music:make-seqn :name "test-seqn"
				 :notes *seqn1-notes*))

(defvar *track1* (music:make-track :instr "guitar"
				   :seqns (list *seqn1*)))

(defvar *my-song* (music:make-song :tempo 120
				   :tracks (list *track1*)))


(jack:with-jack-midi "yoshimi:midi in" ()
		     (music:play-song *my-song*))

