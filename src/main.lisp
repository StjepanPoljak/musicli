(defvar *seqn1-notes* (list (music:make-note :sym 'music::C4 :dur 1)
			    (music:make-note :sym 'music::D4 :dur 1)))

(defvar *seqn1* (music:make-seqn :name "test-seqn"
				 :notes *seqn1-notes*))

(defvar *track1* (music:make-track :instr "guitar"
				   :seqns (list *seqn1*)))

(defvar *my-song* (music:make-song :tempo 120
				   :tracks (list *track1*)))

(jack:with-jack-midi "yoshimi:midi in" ()
  (music:init-song *my-song*)
  (music:set-curr-song *my-song*)
  (sleep 10)
  (music:set-curr-song nil)
  (music:set-start-time 0))
