(in-package "IM")

(reset-im-rules 'postprocessRules2)

(mapcar #'(lambda (x) (add-im-rule x 'postprocessRules2))  ;; sets of rules are tagged so they can be managed independently 
	'(
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ; sequences
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ((ONT::SA-SEQ ?ev ONT::SPEECH-ACT :ACTS (?!a1 ?!a2 ?a3) :OPERATOR ?op)
	   -sa_seq>
	   100
	   (ONT::EVENT ?ev ONT::SA-SEQ
	    :sequence ?!a1
	    :sequence1 ?!a2
	    :sequence2 ?a3
	    :op ?op
	    :rule -sa_seq
	    ))
	  
	  ; two elements only, when both are EVENTs
	  ((?reln ?ev (? t ONT::SITUATION-ROOT) :SEQUENCE (?!s1 ?!s2) :OPERATOR ?op)
	   (ONT::EVENT ?!s1 (? !t1 ONT::NOOP))
	   (ONT::EVENT ?!s2 (? !t2 ONT::NOOP))
	   -event_seq>
	   100
	   (ONT::EVENT ?ev ONT::SEQ
	    :type ?t
	    :sequence (?!s1 ?!s2)
	    :operator ?op
	    :rule -event_seq
	    ))


))
