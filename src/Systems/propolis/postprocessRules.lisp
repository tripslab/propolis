(in-package "IM")

(reset-im-rules 'postprocessRules)

(mapcar #'(lambda (x) (add-im-rule x 'postprocessRules))  ;; sets of rules are tagged so they can be managed independently 
	'(
	  
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  ((ONT::EVENT ?!ev ONT::CREATE :TYPE (:* ONT::CAUSE-EFFECT W::CAUSE) :FORMAL ?!f)
	   -noop>
	   100
	   (ONT::EVENT ?!ev ONT::NOOP
	    :rule -noop
	    ))

	  ; exclude all subtypes of CAUSE-EFFECT (except CAUSE-PRODUCE-REPRODUCE which has its own rule)
	  ((ONT::EVENT ?!ev ONT::CREATE :TYPE (? t ONT::ALLOW ONT::CAUSE-STIMULATE ONT::ENABLE ONT::ENCOURAGE ONT::ENSURE ONT::HAVE-INFLUENCE ONT::HELP ONT::MAINTAIN-KEEP ONT::MAKE-IT-SO ONT::PROVOKE ONT::START ONT::TRY) )
	   -noop2>
	   100
	   (ONT::EVENT ?!ev ONT::NOOP
	    :rule -noop2
	    ))


	  ; exclude some subtypes of CHANGE (some are extracted as higher-priority events, e.g., ONT::DAMAGE)
	  ((ONT::EVENT ?!ev ONT::CHANGE :TYPE (? t ONT::CHANGE-STATE-ACTION ONT::REVIVING ONT::FLUCTUATE ONT::STABILIZE ONT::ADJUST ONT::ADJUST-SURFACE ONT::CHANGE-IN-SCALE ONT::COLORING ONT::CONTINUOUS-CHANGE ONT::COOKING ONT::ENCODE ) )
	   -noop3>
	   100
	   (ONT::EVENT ?!ev ONT::NOOP
	    :rule -noop3
	    ))


	  ; change all other misc F extractions with LOC into EVENT
	  ((ONT::F ?!ev ?t :LOCMOD ?!locmod :LOC ?loc) ; ?loc is optional because "I moved down" has only :LOCMOD but no :LOC
	   -f_to_event>
	   100
	   (ONT::EVENT ?!ev ONT::OTHER
            :TYPE ?t
	    :rule -f_to_event
	    ))
	  
	  ; change all other misc F extractions with LOC1 into EVENT
	  ((ONT::F ?!ev ?t :LOCMOD1 ?!locmod :LOC1 ?loc)
	   -f_to_event2>
	   100
	   (ONT::EVENT ?!ev ONT::OTHER
            :TYPE ?t
	    :rule -f_to_event2
	    ))
	  
	  ; change all other misc F extractions with RES into EVENT
	  ((ONT::F ?!ev ?t :RES ?!res)
	   -f_to_event3>
	   100
	   (ONT::EVENT ?!ev ONT::OTHER
            :TYPE ?t
	    :rule -f_to_event3
	    ))
	  
	  ; change all other misc F extractions with RES1 into EVENT
	  ((ONT::F ?!ev ?t :RES1 ?!res)
	   -f_to_event4>
	   100
	   (ONT::EVENT ?!ev ONT::OTHER
            :TYPE ?t
	    :rule -f_to_event4
	    ))

	  ; change all other non-F misc extractions with LOC into TERM
	  (((? !reln ONT::F ONT::EVENT ONT::TERM ONT::CC ONT::EPI) ?!ev ?t :LOCMOD ?!locmod :LOC ?loc)
	   -term>
	   100
	   (ONT::TERM ?!ev ONT::MISC
            :TYPE ?t
	    :rule -term
	    ))
	  
	  ; change all other non-F misc extractions with LOC1 into TERM
	  (((? !reln ONT::F ONT::EVENT ONT::TERM ONT::CC ONT::EPI) ?!ev ?t :LOCMOD1 ?!locmod :LOC1 ?loc)
	   -term2>
	   100
	   (ONT::TERM ?!ev ONT::MISC
            :TYPE ?t
	    :rule -term2
	    ))
	  
	  ; change all other non-F misc extractions with locations into TERM
	  (((? !reln ONT::F ONT::EVENT ONT::TERM ONT::CC ONT::EPI) ?!ev ?t :RES ?!res)
	   -term3>
	   100
	   (ONT::TERM ?!ev ONT::MISC
            :TYPE ?t
	    :rule -term3
	    ))
	  
	  ; change all other non-F misc extractions with locations into TERM
	  (((? !reln ONT::F ONT::EVENT ONT::TERM ONT::CC ONT::EPI) ?!ev ?t :RES1 ?!res)
	   -term4>
	   100
	   (ONT::TERM ?!ev ONT::MISC
            :TYPE ?t
	    :rule -term4
	    ))
	  
))
