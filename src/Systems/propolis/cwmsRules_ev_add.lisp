(in-package "IM")

(reset-im-rules 'cwms_ev_add)

(mapcar #'(lambda (x) (add-im-rule x 'cwms_ev_add))  
	'(

	  ; The box is in the corner/here.
          ((?!reln0 ?ev
            (? type ONT::HAVE-PROPERTY) :NEUTRAL ?!ag :FORMAL ?!loc :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
	   (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN
		      ONT::GOAL-AS-CONTAINMENT ONT::GOAL-AS-ON ONT::TO ONT::TO-LOC ONT::OBJ-IN-PATH ONT::SOURCE-RELN ;ONT::PATH but not RESULTING-STATE
		      )
		   :GROUND ?!locVal)
		   ;:FIGURE ?ev)
           ;(?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?relnLoc ?!locVal (? !locType ONT::SITUATION-ROOT ONT::SPEECH-ACT ONT::DEFINITENESS ONT::ANY-TIME-OBJECT)) ; allow REFERENTIAL-SEM, e.g., misspelled words(!) and pronouns
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule_have_property>
           20
           (ONT::event ?ev ?type
            :rule -rule_have_property
            :NEUTRAL ?!ag
            :FORMAL -
	    :LOCMOD ?tmp
            :LOC ?!locVal
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )


	  ; The box moved closer to the table.
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :EXTENT ?!ext)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!ext
		   (? extType ONT::EXTENT-PREDICATE)
		   :GROUND ?!tmp)
		   ;:FIGURE ?ev)
           (?reln_tmp ?!tmp (? tmpType ONT::VALUE) :MODS (?!loc))
           (ONT::F ?!loc (? locType ONT::LESS-VAL ONT::MORE-VAL) :GROUND ?!locVal :SCALE ONT::DISTANCE-SCALE)
           ;(?reln_locVal ?!locVal (? locValType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?reln_locVal ?!locVal (? !locValType ONT::SITUATION-ROOT ONT::SPEECH-ACT ONT::DEFINITENESS ONT::ANY-TIME-OBJECT))
           -loc-comparative>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc-comparative
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?!loc
            :LOC ?!locVal
	    :EXTENT -
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            ;:TYPE ?type ; symbolmapping not called; ?type may already have been symbolmapped earlier
            ;:DRUM ?code
            )
           )

	  
	  ;;;;;;;;;;;;;;;;;;;
	  ;;;; LOC rules
	  ;;;;;;;;;;;;;;;;;;;

	  #|
	  ; :FIGURE of ?!loc points to ?ev
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
	    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN ONT::PATH)
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal :FIGURE ?ev)
           (?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -loc1>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc1
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            :LOC ?!locVal
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            :TYPE ?type
            ;:DRUM ?code
            )
           )
	  |#
	  
	  ; this applies to non-events too; e.g., ice in the cup (?reln0 is converted to ONT::TERM in the EKB)
	  ; no FIGURE; :LOCATION points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :LOCATION ?!loc)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN
		      ONT::GOAL-AS-CONTAINMENT ONT::GOAL-AS-ON ONT::TO ONT::TO-LOC ONT::OBJ-IN-PATH ONT::SOURCE-RELN ;ONT::PATH but not RESULTING-STATE
		      )
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal)
		   ;:FIGURE ?ev)
           ;(?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?relnLoc ?!locVal (? !locType ONT::SITUATION-ROOT ONT::SPEECH-ACT ONT::DEFINITENESS ONT::ANY-TIME-OBJECT))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED)) ; should have been symbol-mapped already from previous levels.  If done here, ONT::F types would be changed from (:* ont::type w::word) to just ont::type even if it doesn't match anything in symbolmapping
           -loc2>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc2
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            :LOC ?!locVal
	    :LOCATION -
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            ;:TYPE ?type ; symbolmapping not called; ?type may already have been symbolmapped earlier
            ;:DRUM ?code
            )
           )

	  ; this applies to non-events; e.g., ice from the cup.  
	  ; no FIGURE; :MOD points to SOURCE-AS-LOC
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :MOD ?!loc)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::SOURCE-AS-LOC)
		   ;(? tmp ONT::POSITION-RELN ONT::PATH)
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal)
		   ;:FIGURE ?ev)
           ;(?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?relnLoc ?!locVal (? !locType ONT::SITUATION-ROOT ONT::SPEECH-ACT ONT::DEFINITENESS ONT::ANY-TIME-OBJECT))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED)) ; should have been symbol-mapped already from previous levels.  If done here, ONT::F types would be changed from (:* ont::type w::word) to just ont::type even if it doesn't match anything in symbolmapping
           -loc2-mod>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc2-mod
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            :LOC ?!locVal
	    :LOCATION -
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            ;:TYPE ?type ; symbolmapping not called; ?type may already have been symbolmapped earlier
            ;:DRUM ?code
            )
           )
	  
	  ; I ate the pizza in the kitchen ("in the kitchen" is attached to "pizza")
	  ; no FIGURE; :LOCATION of :AFFECTED points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
		    ;:AGENT ?!ag
		    :AFFECTED ?!obj ;:DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :passive -
		    )
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART) :LOCATION ?!loc)
           (?reln2 ?!obj (? !t2 ONT::SITUATION-ROOT ONT::SPEECH-ACT ONT::DEFINITENESS ONT::ANY-TIME-OBJECT) :LOCATION ?!loc)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN
		      ONT::GOAL-AS-CONTAINMENT ONT::GOAL-AS-ON ONT::TO ONT::TO-LOC ONT::OBJ-IN-PATH ONT::SOURCE-RELN ;ONT::PATH but not RESULTING-STATE
		      )
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal
		   :FIGURE ?!obj)
           ;(?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?relnLoc ?!locVal (? !locType ONT::SITUATION-ROOT ONT::SPEECH-ACT ONT::DEFINITENESS ONT::ANY-TIME-OBJECT))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -loc2-AFFECTED>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc2-AFFECTED
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            :LOC ?!locVal
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            ;:TYPE ?type
            ;:DRUM ?code
            )
           )

	  
	  ; no FIGURE; :SOURCE points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :SOURCE ?!loc)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN
		      ONT::GOAL-AS-CONTAINMENT ONT::GOAL-AS-ON ONT::TO ONT::TO-LOC ONT::OBJ-IN-PATH ONT::SOURCE-RELN ;ONT::PATH but not RESULTING-STATE
		      )
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal)
		   ;:FIGURE ?ev)
           ;(?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?relnLoc ?!locVal (? !locType ONT::SITUATION-ROOT ONT::SPEECH-ACT ONT::DEFINITENESS ONT::ANY-TIME-OBJECT))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -loc3>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc3
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            :LOC ?!locVal
	    :SOURCE -
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            ;:TYPE ?type
            ;:DRUM ?code
            )
           )

	  #| this is now split into two rules so we can zero out the :RESULT and/or :RESULT1
	  ; no FIGURE; :RESULT points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :RESULT ?!loc :RESULT1 ?!loc1)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN ONT::PATH)
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal)
		   ;:FIGURE ?ev)
           (?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?!loc1
		   (? tmp1 ONT::POSITION-RELN ONT::PATH)
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal1)
		   ;:FIGURE ?ev)
           (?relnLoc1 ?!locVal1 (? locType1 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -loc4>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc4
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            :LOC ?!locVal
	    :LOCMOD1 ?tmp1
	    :LOC1 ?!locVal1
	    :RESULT -
	    :RESULT1 -
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            :TYPE ?type
            ;:DRUM ?code
            )
           )
	  |#

	  ; no FIGURE; :RESULT points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :RESULT ?!loc)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN
		      ONT::GOAL-AS-CONTAINMENT ONT::GOAL-AS-ON ONT::TO ONT::TO-LOC ONT::OBJ-IN-PATH ONT::SOURCE-RELN ;ONT::PATH but not RESULTING-STATE
		      )
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal)
		   ;:FIGURE ?ev)
           ;(?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?relnLoc ?!locVal (? !locType ONT::SITUATION-ROOT ONT::SPEECH-ACT ONT::DEFINITENESS ONT::ANY-TIME-OBJECT))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -loc4a>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc4a
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            :LOC ?!locVal
	    :RESULT -
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            ;:TYPE ?type
            ;:DRUM ?code
            )
           )

	  ; no FIGURE; :RESULT1 points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :RESULT1 ?!loc)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN
		      ONT::GOAL-AS-CONTAINMENT ONT::GOAL-AS-ON ONT::TO ONT::TO-LOC ONT::OBJ-IN-PATH ONT::SOURCE-RELN ;ONT::PATH but not RESULTING-STATE
		      )
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal)
		   ;:FIGURE ?ev)
           ;(?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?relnLoc ?!locVal (? !locType ONT::SITUATION-ROOT ONT::SPEECH-ACT ONT::DEFINITENESS ONT::ANY-TIME-OBJECT))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -loc4b>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc4b
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD1 ?tmp
            :LOC1 ?!locVal
	    :RESULT1 -
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            ;:TYPE ?type
            ;:DRUM ?code
            )
           )

	  ; no FIGURE; :RESULT2 points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :RESULT2 ?!loc)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN
		      ONT::GOAL-AS-CONTAINMENT ONT::GOAL-AS-ON ONT::TO ONT::TO-LOC ONT::OBJ-IN-PATH ONT::SOURCE-RELN ;ONT::PATH but not RESULTING-STATE
		      )
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal)
		   ;:FIGURE ?ev)
           ;(?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?relnLoc ?!locVal (? !locType ONT::SITUATION-ROOT ONT::SPEECH-ACT ONT::DEFINITENESS ONT::ANY-TIME-OBJECT))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -loc4c>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc4c
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD2 ?tmp
            :LOC2 ?!locVal
	    :RESULT2 -
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            ;:TYPE ?type
            ;:DRUM ?code
            )
           )
	  
	  ; ONT::DIRECTION with no GROUND (only extract :LOCMOD, no :LOC)
	  ; e.g., I put the box down.	
	  ; 
	  ; no FIGURE; :RESULT points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :RESULT ?!loc)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::DIRECTION)
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND -)
		   ;:FIGURE ?ev)
           ;(?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -loc5a>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc5a
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            ;:LOC ?!locVal
	    :RESULT -
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            ;:TYPE ?type
            ;:DRUM ?code
            )
           )
	  
	  ; ONT::DIRECTION with no GROUND (only extract :LOCMOD, no :LOC)
	  ; e.g., I put the box down.	
	  ; 
	  ; no FIGURE; :RESULT1 points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :RESULT1 ?!loc)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::DIRECTION)
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND -)
		   ;:FIGURE ?ev)
           ;(?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -loc5b>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc5b
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            ;:LOC ?!locVal
	    :RESULT1 -
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            ;:TYPE ?type
            ;:DRUM ?code
            )
           )

	  ; ONT::DIRECTION with no GROUND (only extract :LOCMOD, no :LOC)
	  ; e.g., I put the box down.	
	  ; 
	  ; no FIGURE; :RESULT2 points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :RESULT2 ?!loc)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::DIRECTION)
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND -)
		   ;:FIGURE ?ev)
           ;(?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -loc5c>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc5c
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            ;:LOC ?!locVal
	    :RESULT2 -
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            ;:TYPE ?type
            ;:DRUM ?code
            )
           )
	  
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;; (un)able to => can(not)
	  ;;; need rules for other roles (NEUTRAL, etc)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  ; I am able to eat (AGENT)
          ((?!reln0 ?ev ONT::ABLE :FIGURE ?!fig :GROUND ?!gd)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?!reln1 ?!fig ?!fig-type)
           (?!reln2 ?!gd ?!gd-type :passive -)
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -able>
           40
           (?!reln2 ?!gd ?!gd-type
            :rule -able
	    :AGENT ?!fig
	    :MODALITY (:* ONT::ABILITY W::CAN)
	    :FORCE ONT::POSSIBLE
            )
           )

	  ; I am unable to eat the pizza (AGENT)
          ((?!reln0 ?ev ONT::UNABLE :FIGURE ?!fig :GROUND ?!gd)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?!reln1 ?!fig ?!fig-type)
           (?!reln2 ?!gd ?!gd-type :passive -)
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -able2>
           40
           (?!reln2 ?!gd ?!gd-type
            :rule -able2
	    :AGENT ?!fig
	    :MODALITY (:* ONT::ABILITY W::CAN)
	    :FORCE ONT::IMPOSSIBLE
	    :NEGATION +
            )
           )

	  ; The pizza is able to be eaten (AFFECTED)
          ((?!reln0 ?ev ONT::ABLE :FIGURE ?!fig :GROUND ?!gd)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?!reln1 ?!fig ?!fig-type)
           (?!reln2 ?!gd ?!gd-type :passive +)
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -able3>
           40
           (?!reln2 ?!gd ?!gd-type
            :rule -able3
	    :AFFECTED ?!fig
	    :MODALITY (:* ONT::ABILITY W::CAN)
	    :FORCE ONT::POSSIBLE
            )
           )

	  ; The pizza is unable to be eaten (AFFECTED)
          ((?!reln0 ?ev ONT::UNABLE :FIGURE ?!fig :GROUND ?!gd)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?!reln1 ?!fig ?!fig-type)
           (?!reln2 ?!gd ?!gd-type :passive +)
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -able4>
           40
           (?!reln2 ?!gd ?!gd-type
            :rule -able4
	    :AFFECTED ?!fig
	    :MODALITY (:* ONT::ABILITY W::CAN)
	    :FORCE ONT::IMPOSSIBLE
	    :NEGATION +
            )
           )

#|
	  ;;;;;;;;;;;;;;;;;;;
	  ;;;; provenance      ; move to rules_mod
	  ;;;;;;;;;;;;;;;;;;;

	  ; He said he ate the pizza.
	  ((?!reln0 ?ev ONT::COMMUNICATION :AGENT ?!ag :FORMAL ?!f)
           (?!reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln2 ?!f ?t2)
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -attributedTo>
           40
           (?!reln2 ?!f ?t2
            :rule -attributedTo
	    :ATTRIBUTED-TO ?!ag
            )
           )

	  ; "I ate the pizza," he said.             ; need the speechact too for questions etc: maybe pass on the speechact?
          ((?!reln0 ?ev ONT::COMMUNICATION :AGENT ?!ag :FORMAL ?!f)
           (?!reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::SPEECHACT ?!f ?t2 :CONTENT ?!c)
	   (?!reln3 ?!c ?t3)
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -attributedTo2>
           40
           (?!reln3 ?!c ?t3
            :rule -attributedTo2
	    :ATTRIBUTED-TO ?!ag
            )
           )
	  
	  
	  ;;;;;;;;;;;;;;;;;;;
	  ;;;; TIME rules
	  ;;;;;;;;;;;;;;;;;;;

	  ; I ate Saturday.
	  ; I ate in January.
	  ; I ate beginning Saturday.
	  ; I ate yesterday.
	  ; I ate in recent weeks.
          ((?!reln0 ?ev ?type :TIME ?!time)
	   (?!reln1 ?!time (? time-type ONT::TEMPORAL-PREDICATE) :GROUND ?!gd)
           -time>
           40 ;30
           (?!reln0 ?ev ?type 
            :rule -time
	    :TIMEMOD ?time-type
	    :TIME ?!gd
            )
           )
	  
	  #|
	  ; I ate Saturday.
          ((?!reln0 ?ev ?type :TIME ?!time)
	   (?!reln1 ?!time (? time-type ONT::TEMPORAL-PREDICATE) :GROUND ?!gd)
	   (?!reln2 ?!gd (? gd-type ONT::TIME-LOC) :DAY-OF-WEEK ?!day)
           -time1>
           40
           (?!reln0 ?ev ?type 
            :rule -time1
	    :TIMEMOD ?time-type
	    :TIME ?!day
            )
           )

	  ; I ate in January.
          ((?!reln0 ?ev ?type :TIME ?!time)
	   (?!reln1 ?!time (? time-type ONT::TEMPORAL-PREDICATE) :GROUND ?!gd)
	   (?!reln2 ?!gd (? gd-type ONT::TIME-LOC) :MONTH ?!month)
           -time2>
           40
           (?!reln0 ?ev ?type 
            :rule -time2
	    :TIMEMOD ?time-type
	    :TIME ?!month
            )
           )
	  |#


|#
	  
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  )
	)
