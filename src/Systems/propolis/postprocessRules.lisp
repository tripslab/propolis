(in-package "IM")

(reset-im-rules 'postprocessRules)

(mapcar #'(lambda (x) (add-im-rule x 'postprocessRules))  ;; sets of rules are tagged so they can be managed independently 
	'(	  
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

	  ; do not extract spin/turn/etc without an additional modifier (e.g. "spin away")
	  ((ONT::EVENT ?ev ONT::MOVE :LOCMOD - :TO - :FROM - :TYPE ONT::ROTATE)
	   -noop4>
	   100
	   (ONT::EVENT ?ev ONT::NOOP
	    :rule -noop4
	    ))

	  ; remove "at high pressure" from LOCMOD/LOC
	  ((?reln0 ?ev ?t0 :LOCMOD ?locmod :LOC ?!loc)
	   (?reln1 ?!loc (:* ?!t1 (? w w::pressure w::pressures)))
	   -rm>
	   100
	   (?reln0 ?ev ?t0
	    :LOCMOD -
   	    :LOC -
	    :rule -rm
	    ))

	  ; remove "at high temperature" etc from LOCMOD/LOC
	  ((?reln0 ?ev ?t0 :LOCMOD ?locmod :LOC ?!loc)
	   (?reln1 ?!loc ONT::MEASURE-SCALE)
	   -rm2>
	   100
	   (?reln0 ?ev ?t0
	    :LOCMOD -
   	    :LOC -
	    :rule -rm2
	    ))
	  

	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  ; change all other misc F extractions with LOC into EVENT
	  ((ONT::F ?!ev ?t :LOCMOD ?!locmod :LOC ?loc) ; ?loc is optional because "I moved down" has only :LOCMOD but no :LOC
	   -f_to_event>
	   100
	   (ONT::EVENT ?!ev ?t ;ONT::OTHER
            :TYPE ?t
	    :rule -f_to_event
	    ))
	  
	  ; change all other misc F extractions with LOC1 into EVENT
	  ((ONT::F ?!ev ?t :LOCMOD1 ?!locmod :LOC1 ?loc)
	   -f_to_event2>
	   100
	   (ONT::EVENT ?!ev ?t ;ONT::OTHER
            :TYPE ?t
	    :rule -f_to_event2
	    ))
	  
	  ; change all other misc F extractions with LOC into EVENT
	  ((ONT::F ?!ev ?t :from ?!from) ; ?loc is optional because "I moved down" has only :LOCMOD but no :LOC
	   -f_to_event3>
	   100
	   (ONT::EVENT ?!ev ?t ;ONT::OTHER
            :TYPE ?t
	    :rule -f_to_event3
	    ))
	  
	  ; change all other misc F extractions with LOC into EVENT
	  ((ONT::F ?!ev ?t :to ?!to) ; ?loc is optional because "I moved down" has only :LOCMOD but no :LOC
	   -f_to_event4>
	   100
	   (ONT::EVENT ?!ev ?t ;ONT::OTHER
            :TYPE ?t
	    :rule -f_to_event4
	    ))
	  
	  ; change all other misc F extractions with RES into EVENT
	  ((ONT::F ?!ev ?t :RES ?!res)
	   -f_to_event5>
	   100
	   (ONT::EVENT ?!ev ?t ;ONT::OTHER
            :TYPE ?t
	    :rule -f_to_event5
	    ))
	  
	  ; change all other misc F extractions with RES1 into EVENT
	  ((ONT::F ?!ev ?t :RES1 ?!res)
	   -f_to_event6>
	   100
	   (ONT::EVENT ?!ev ?t ;ONT::OTHER
            :TYPE ?t
	    :rule -f_to_event6
	    ))

	  
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	  
	  
	  ; change all other non-F misc extractions with LOC into TERM
	  (((? !reln ONT::F ONT::EVENT ONT::TERM ONT::CC ONT::EPI) ?!ev ?t :LOCMOD ?!locmod :LOC ?loc)
	   -term>
	   100
	   (ONT::TERM ?!ev ?t ;ONT::MISC
            :TYPE ?t
	    :rule -term
	    ))
	  
	  ; change all other non-F misc extractions with LOC1 into TERM
	  (((? !reln ONT::F ONT::EVENT ONT::TERM ONT::CC ONT::EPI) ?!ev ?t :LOCMOD1 ?!locmod :LOC1 ?loc)
	   -term2>
	   100
	   (ONT::TERM ?!ev ?t ;ONT::MISC
            :TYPE ?t
	    :rule -term2
	    ))
	  
	  ; change all other non-F misc extractions with LOC into TERM
	  (((? !reln ONT::F ONT::EVENT ONT::TERM ONT::CC ONT::EPI) ?!ev ?t :from ?!from)
	   -term3>
	   100
	   (ONT::TERM ?!ev ?t ;ONT::MISC
            :TYPE ?t
	    :rule -term3
	    ))
	  
	  ; change all other non-F misc extractions with LOC1 into TERM
	  (((? !reln ONT::F ONT::EVENT ONT::TERM ONT::CC ONT::EPI) ?!ev ?t :to ?!to)
	   -term4>
	   100
	   (ONT::TERM ?!ev ?t ;ONT::MISC
            :TYPE ?t
	    :rule -term4
	    ))
	  
	  ; change all other non-F misc extractions with locations into TERM
	  (((? !reln ONT::F ONT::EVENT ONT::TERM ONT::CC ONT::EPI) ?!ev ?t :RES ?!res)
	   -term5>
	   100
	   (ONT::TERM ?!ev ?t ;ONT::MISC
            :TYPE ?t
	    :rule -term5
	    ))
	  
	  ; change all other non-F misc extractions with locations into TERM
	  (((? !reln ONT::F ONT::EVENT ONT::TERM ONT::CC ONT::EPI) ?!ev ?t :RES1 ?!res)
	   -term6>
	   100
	   (ONT::TERM ?!ev ?t ;ONT::MISC
            :TYPE ?t
	    :rule -term6
	    ))

	  ; change all other non-F misc extractions with LOCATION into TERM
	  ; sometimes -loc2> in cwmsRules_ev_add does this conversion too, but since we are lifting the location to the EVENT too (using -loc2-AFFECTED> for example), the latter covers more LF clauses and so the LOCATION is only lifted into the EVENT and not to the TERM also (e.g., "Droplets in the clouds collide")
	  (((? !reln ONT::F ONT::EVENT ONT::TERM ONT::CC ONT::EPI) ?!ev ?t :LOCATION ?!loc)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN
		      ONT::GOAL-AS-CONTAINMENT ONT::GOAL-AS-ON ONT::TO ONT::TO-LOC ONT::OBJ-IN-PATH ONT::SOURCE-RELN ;ONT::PATH but not RESULTING-STATE
		      )
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal)
		   ;:FIGURE ?ev)
           ;(?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?relnLoc ?!locVal (? !locType ONT::SITUATION-ROOT ONT::SPEECH-ACT ONT::DEFINITENESS ONT::ANY-TIME-OBJECT))	   
	   -term7>
	   100
	   (ONT::TERM ?!ev ?t ;ONT::MISC
            :TYPE ?t
	    :LOCMOD ?tmp
            :LOC ?!locVal
	    :LOCATION -

	    :rule -term7
	    ))
	  
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  
))
