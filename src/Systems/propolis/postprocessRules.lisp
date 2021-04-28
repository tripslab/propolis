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
	  
))
