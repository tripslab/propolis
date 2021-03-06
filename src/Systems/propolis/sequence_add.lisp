(in-package "IM")

(reset-im-rules 'sequence_add)

(mapcar #'(lambda (x) (add-im-rule x 'sequence_add))  
	'(

	  ; I pushed the pizza quickly and into the oven.  (extract only "into the oven")
	  ; I pushed the pizza into the oven or onto the table (the "or" is extracted as :OPERATOR)
	  ; both -loc_seq> and -loc_seq1> fire: -loc_seq> extracts the first element and -loc_seq1> extracts the second element
	  ; to fix: only 2-element sequences
	  
	  ; no FIGURE; :RESULT points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
	  ; extract first element of sequence
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :RESULT ?!loc_seq)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
	   (ONT::F ?!loc_seq ?loc_seq_type :sequence (?!loc ?!seq1) :OPERATOR ?!op)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN ONT::PATH)
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal)
					;:FIGURE ?ev)
           (?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
	   (?reln1 ?!seq1 (? !tmp1 ONT::POSITION-RELN ONT::PATH))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -loc_seq>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc_seq
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            :LOC ?!locVal
	    :RESULT -
	    ;:OPERATOR ?!op ; the :OPERATOR is extracted explicitly because the ?!loc_seq clause would be lost (unreachable)---this is only extracted when both sequence elements are locations
	    
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            ;:TYPE ?type
            ;:DRUM ?code
            )
           )


	  ; no FIGURE; :RESULT points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
	  ; extract second element of sequence
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :RESULT ?!loc_seq)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
	   (ONT::F ?!loc_seq ?loc_seq_type :sequence (?!seq0 ?!loc) :OPERATOR ?!op)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN ONT::PATH)
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal)
		   ;:FIGURE ?ev)
           (?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
	   (?reln0 ?!seq0 (? !tmp0 ONT::POSITION-RELN ONT::PATH))
           -loc_seq1>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc_seq1
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD1 ?tmp   ; use LOCMOD1/LOC1 here.  Otherwise it would be overwritten by the extraction from -loc-seq because both the loc-seq and loc-seq1 extractions have the same ?ev
            :LOC1 ?!locVal
	    :RESULT -
	    ;:OPERATOR ?!op
	    
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            ;:TYPE ?type
            ;:DRUM ?code
            )
           )

	  
	  ; no FIGURE; :RESULT points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
	  ; extract first element of sequence
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :RESULT ?!loc_seq)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
	   (ONT::F ?!loc_seq ?loc_seq_type :sequence (?!loc ?!loc1) :OPERATOR ?!op)
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
           -loc_seq_both>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc_seq_both
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            :LOC ?!locVal
            :LOCMOD1 ?tmp1   ; use LOCMOD1/LOC1 here.  Otherwise it would be overwritten by the extraction from -loc-seq because both the loc-seq and loc-seq1 extractions have the same ?ev
            :LOC1 ?!locVal1
	    :RESULT -
	    :LOC_OPERATOR ?!op ; the :OPERATOR is extracted explicitly because the ?!loc_seq clause would be lost (unreachable)
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            ;:TYPE ?type
            ;:DRUM ?code
            )
           )
	  
	  ))

