(in-package "IM")

(reset-im-rules 'drum_ev_add)

(mapcar #'(lambda (x) (add-im-rule x 'drum_ev_add))  
	'(
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  ;;;;;;;;;;;;;;;;;;;
	  ;;;; result rules
	  ;;;;;;;;;;;;;;;;;;;
	  
	  ; RAS converts GTP into/to GDP. 
	  ((ONT::EVENT ?ev ?type :RESULT ?!res)
	   (ONT::F ?!res (? type1 ont::goal-reln ont::resulting-object) :GROUND ?!res1)  ; resulting-state is in goal-reln (but resulting-state would have been extracted as a LOCMOD already so not useful here) ; update: resulting-state is excluded from location extractions now
	   #|
	   (;(? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM)
	    ?reln1
	    ?!res1  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART
;ONT::CELL-PART ONT::BODY-PART ONT::MEDICAL-DISORDERS-AND-CONDITIONS
ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ))
	   |#
           -res1>
           100
	   (ONT::EVENT ?ev ?type
	    :rule -res1
	    :RES ?!res1
	    :RESULT -
	    )
          )

	  ; result1
	  ; RAS converts GTP into/to GDP. 
	  ((ONT::EVENT ?ev ?type :RESULT1 ?!res)
	   (ONT::F ?!res (? type1 ont::goal-reln ont::resulting-object) :GROUND ?!res1)  ; resulting-state is in goal-reln (but resulting-state would have been extracted as a LOCMOD already so not useful here) ; update: resulting-state is excluded from location extractions now
	   #|
	   (;(? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM)
	    ?reln1
	    ?!res1  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART
;ONT::CELL-PART ONT::BODY-PART ONT::MEDICAL-DISORDERS-AND-CONDITIONS
ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ))
	   |#
           -res1b>
           100
	   (ONT::EVENT ?ev ?type
	    :rule -res1b
	    :RES1 ?!res1
	    :RESULT1 -
	    )
          )
	  
	  ; RAS converts GTP to form GDP. 
	  ((ONT::EVENT ?ev ?type :REASON ?!res)
	   (ONT::F ?!res ONT::PURPOSE :GROUND ?!res1)
	   (ONT::EVENT ?!res1 ONT::PRODUCE :AFFECTED-RESULT ?!res2)
	   #|
	   (;(? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM)
	    ?reln1
	    ?!res2  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART
;ONT::CELL-PART ONT::BODY-PART ONT::MEDICAL-DISORDERS-AND-CONDITIONS
ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ))
	   |#
           -res2>
           100
	   (ONT::EVENT ?ev ?type
	    :rule -res2
	    :RES ?!res2
	    :REASON -  ; get rid of this; otherwise we get a CC BY-MEANS-OF using -PURPOSE-GD>
	    )
          )

	  ; RAS phosphorylates GTP into/to GDP. 
	  ((ONT::EVENT ?ev ?type :MODS (?!res))
	   (ONT::F ?!res (? type1 ont::goal-reln  ont::resulting-object) :GROUND ?!res1)
	   #|
	   (;(? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM)
	    ?reln1
	    ?!res1  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART
;ONT::CELL-PART ONT::BODY-PART ONT::MEDICAL-DISORDERS-AND-CONDITIONS
ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ))
	   |#
           -res3>
           100
	   (ONT::EVENT ?ev ?type
	    :rule -res3
	    :RES ?!res1
	    )
          )

	  #|
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  ; Ras has no effect on Raf.
	  ((ONT::EVENT ?ev ?type :QUAN ONT::NONE)
	   -quan-none>
           100
	   (ONT::EVENT ?ev ?type
	    :rule -quan-none>
	    :negation + 
	    )
          )

	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  ; the downregulation of the gene
	  ((ONT::EVENT ?ev ONT::INHIBIT :TYPE ONT::DOWNREGULATE)
	   -downregulate>
           100
	   (ONT::EVENT ?ev ONT::MODULATE
	    :polarity ONT::NEGATIVE
	    :rule -downregulate>
	    )
          )
	  |#


	  ; It becomes a toad.
	  ((ONT::EVENT ?ev ONT::CHANGE :TYPE ONT::BECOME :AFFECTED1 ?!res)
           -res4>
           100
	   (ONT::EVENT ?ev ONT::CHANGE
	    :rule -res4
	    :RES ?!res
	    :AFFECTED1 -
	    )
          )



	  ;;;;;;;;;;;;;;;;;;;
	  ;;;; source rules, mostly for CHANGE EVENTs
	  ;;;;;;;;;;;;;;;;;;;

	  ; Is this rule subsumed by -src4? (except that SOURCE-RELN is more general)
	  ; The pizza is converted from dough. 
	  ((ONT::EVENT ?ev ONT::CHANGE :SOURCE ?!res)
	   (ONT::F ?!res (? type1 ont::source-reln) :GROUND ?!res1)  ; resulting-state is in goal-reln
	   #|
	   (;(? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM)
	    ?reln1
	    ?!res1  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART
;ONT::CELL-PART ONT::BODY-PART ONT::MEDICAL-DISORDERS-AND-CONDITIONS
ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ))
	   |#
           -src1>
           100
	   (ONT::EVENT ?ev ONT::CHANGE
	    :rule -src1
	    :SRC ?!res1
	    :SOURCE -
	    )
          )
	  
	  ; The doll is made from paper.
	  ((ONT::EVENT ?ev ?type :MODS (?!res))
	   (ONT::F ?!res (? type1 ont::original-material) :GROUND ?!res1)
	   #|
	   (;(? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET ONT::TERM)
	    ?reln1
	    ?!res1  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART
;ONT::CELL-PART ONT::BODY-PART ONT::MEDICAL-DISORDERS-AND-CONDITIONS
ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ))
	   |#
           -src3>
           100
	   (ONT::EVENT ?ev ?type
	    :rule -src3
	    :SRC ?!res1
	    )
          )

	  ; remap LOC/LOCMOD to SRC for CHANGE EVENTs
	  ; It changed from a toad to a prince.
	  ((ONT::EVENT ?ev ONT::CHANGE :LOCMOD ONT::SOURCE-AS-LOC :LOC ?!res1)
	   (?!reln1 ?!res1 ?!t)  ; need this clause so it is not subsumbed by -res1>; note that there is no clause corresponding to SOURCE-AS-LOC
           -src4>
           100
	   (ONT::EVENT ?ev ONT::CHANGE
	    :rule -src4
	    :SRC ?!res1
	    :LOC -
	    :LOCMOD -
	    )
	   )

	  ;;;;;;;;;;;;;;;;;;;
	  ;;;; remap locations to to/from
	  ;;;;;;;;;;;;;;;;;;;

	  ; change NEUTRAL role to location for enter/leave/exit/etc
	  ((ONT::EVENT ?!ev ONT::MOVE :TYPE (? t ONT::DEPART) :NEUTRAL ?!n)
	   -enter-depart>
	   100
	   (ONT::EVENT ?!ev ONT::MOVE
	    :from ?!n
	    :neutral -
	    :rule -enter-depart
	    ))

	  ; change NEUTRAL role to location for enter/leave/exit/etc
	  ((ONT::EVENT ?!ev ONT::MOVE :TYPE (? t ONT::ENTERING) :NEUTRAL ?!n)
	   -enter-depart2>
	   100
	   (ONT::EVENT ?!ev ONT::MOVE
	    ;:locmod (:* ONT::GOAL-AS-CONTAINMENT W::INTO)
	    :to ?!n
	    :neutral -
	    :rule -enter-depart2
	    ))

	  ((?reln ?!ev ?t
		  :locmod (:* (? locmod ONT::SOURCE-RELN ONT::OUTSIDE ONT::OFF) (? !w w::outside w::outside-of))
		  :loc ?!n)
	   (?reln1 ?!n ?t1)  ; extra clause so that it is not the same as -to>
	   -from>
	   100
	   (?reln ?!ev ?t
	    :from ?!n
	    :locmod -
	    :loc -
	    :rule -from
	    ))

	  #|
	  ; what is this for?  This is subsumed by -from>
	  ; I must be working on something and then have forgotten this
	  (((? reln ONT::EVENT ONT::F) ?!ev ?t
		  :locmod (:* (? locmod ONT::SOURCE-RELN ONT::OUTSIDE ONT::OFF) (?! w w::outside w::outside-of))
		  :loc ?!n)
	   (?reln1 ?!n ?t1)  ; extra clause so that it is not the same as -to>
	   -from_b>
	   100
	   (?reln ?!ev ?t
	    :from ?!n
	    :locmod -
	    :loc -
	    :rule -from_b
	    ))
	  |#
	  
	  ((?reln ?!ev ?t :locmod (? locmod ONT::GOAL-AS-CONTAINMENT ONT::GOAL-AS-ON ONT::TO ONT::TO-LOC) :loc ?!n)
	   (?reln1 ?!n ?t1)  ; extra clause so that it is not the same as -from>
	   -to>
	   100
	   (?reln ?!ev ?t
	    :to ?!n
	    :locmod -
	    :loc -
	    :rule -to
	    ))

	  ((?reln ?!ev ?t :locmod1 (? locmod ONT::SOURCE-RELN ONT::OUTSIDE ONT::OFF) :loc1 ?!n)
	   (?reln1 ?!n ?t1)  ; extra clause so that it is not the same as -to>
	   -from1>
	   100
	   (?reln ?!ev ?t
	    :from ?!n
	    :locmod1 -
	    :loc1 -
	    :rule -from1
	    ))

	  ((?reln ?!ev ?t :locmod1 (? locmod ONT::GOAL-AS-CONTAINMENT ONT::GOAL-AS-ON ONT::TO ONT::TO-LOC) :loc1 ?!n)
	   (?reln1 ?!n ?t1)  ; extra clause so that it is not the same as -to>
	   -to1>
	   100
	   (?reln ?!ev ?t
	    :to ?!n
	    :locmod1 -
	    :loc1 -
	    :rule -to1
	    ))
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ; time 

	  ; I ate Saturday.
	  ; I ate in January.
	  ; I ate beginning Saturday.
	  ; I ate yesterday.
	  ; I ate in recent weeks.
          ((?!reln0 ?ev ?type :TIME ?!time)
	   (?!reln1 ?!time (? time-type ONT::TEMPORAL-PREDICATE) :GROUND ?!gd)
	   (?reln2 ?!gd ?t2)
           -time>
           100
           (?!reln0 ?ev ?type 
            :rule -time
	    :TIMEMOD ?time-type
	    :TIME ?!gd
            )
           )
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  )
	)
