(music:init-musicli-state)

(instr:add-out-client "yoshimi:midi in")
(instr:add-instr "organ" "yoshimi:midi in" 60 32)
(instr:add-instr "plucked" "yoshimi:midi in" 70 32)

(defvar *seqn1-notes* (list (music:make-note :sym 'music::C6 :dur 1)
			    (music:make-note :sym (list 'music::B5 'music::D6) :dur 1)
			    (music:make-note :sym (chords::create 'music::A5 'chords::min) :dur 1)))
(defvar *seqn2-notes* (list (music:make-note :sym 'music::C6 :dur 0.25)
			    (music:make-note :sym 'music::G6 :dur 0.25)
			    (music:make-note :sym 'music::B5 :dur 0.25)))

;(format t "~A~%" *seqn1-notes*)

(defvar *seqn1* (music:make-seqn :name "test-seqn"
				 :notes *seqn1-notes*))

(defvar *seqn2* (music:make-seqn :name "test-seqn2"
				 :notes *seqn2-notes*))

(defvar *track1* (music:make-track :instr-name "organ"
				   :seqns (list *seqn1*)))

(defvar *track2* (music:make-track :instr-name "plucked"
				   :seqns (list *seqn2*)))

(defvar *my-song* (music:make-song :tempo 120
				   :tracks (list *track1* *track2*)))

(jack:with-jack-midi "yoshimi:midi in" ()
		     (music:play-song *my-song*))

