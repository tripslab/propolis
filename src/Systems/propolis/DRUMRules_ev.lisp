
;; THIS file contains examples of rules for matching LF forms to extract desired information
;; The general format of the rules is
;;     (<LF pattern>* <atom as rule name> <result>)
;;     where <result> must be of form (<atom> <atom> <atom> [<keyword> <expression>]*) i..e., same format as LF expressions

;;   to use, load a file like this one, and then call (in the IM package)
;;      (ENTAIL "this is a test" 'your-name-here)
;;  Or you can pass in the results of preprocessing (from texttagger or speech recognition)
;;      (ENTAIL '((WORD this :frame (0 3) :pos ART ) (WORD is :frame (4 6) :POS V) ....))) 'your-name-here)

;; to get tracing information out, ll (im::TRACE-ON n), where n is a small number (the higher the more info
;;      you get up to about 5)

(in-package "IM")

; This is now set in system.lisp
;(setq *compute-force-from-tmas* nil) ;; disable interpretation of force (which eliminates modality and tense information)

(setq *extractor-oblig-features* '(:rule))  ; this is so that every :rule mentioned would be passed on
				   
#|
;(setq *extractor-oblig-features* '(:tense :progr :modality :perf :negation :passive :coref :while :until :manner :purpose :domain-specific-info))
(setq *extractor-oblig-features* '(:tense :negation
; :modality
; :degree :frequency 
:progr :perf :passive 
; :coref 
:while :until :purpose 
;:drum    ; we don't automatically pass :drum on because we are constructing logical sequences explicitly in, e.g., -explicit-ref-seq3 (Drumterms)
:ASSOC-WITH  ; for PTM arguments and sites (whcih are chained by :ASSOC-WITH and :MOD)
:MOD
:METHOD ; by-means-of
:REASON ; because
:MANNER ; as a result of
:OF     ; as a result of
;:LOCATION ; molecular-site
:PARENTHETICAL ; in-event (NRAS, bound to GTP, binds BRAF.), also mutation, e.g., -simple-ref-mutation2
:LOC       ; Ras in the nucleus/cell line
:LOCATION  ; Ras in the nucleus/cell line
))
|#

; reset in bob and cwms
(setq *roles-to-emit* '(			    
    :AGENT :AGENT1 :AFFECTED :AFFECTED1 :AFFECTED-RESULT :NEUTRAL :NEUTRAL1 :NEUTRAL2 :EXPERIENCER :FORMAL ; :RESULT (this is used for matching but is converted to :OUTCOME, as in the CC ENABLE)
    :DEGREE :FREQUENCY 
    :SITE :SITEMOD
    :LOC :LOC_MOD :FROM :TO 
    :CELL-LINE
    :POLARITY
    :MODA :MODN
    :DRUM :TYPE
    ;:LEX ; :WNSENSE (this is often misleading)
    :MODALITY :FORCE
    :TENSE :NEGATION :PROGR :PERF :PASSIVE
    ; from drumterms
    :NAME :MUTATION :logicalOp-sequence :OPERATOR :BASE :PRO :m-sequence
    ; from drumterms_add
    :ACTIVE ; :DEGREE :LOC :SITE :CELL-LINE :MODA :MODN  (already in list for EVENTs above)
    ; from drumrules_mod
    :EVENT :EPI :COND
    ; from drumrules_cc
    :INEVENT :FACTOR :FACTOR-SEQUENCE :OUTCOME
    :RES ; for "RAS converts GTP into GDP."
    :COREF
;    :SPEC
    :RULE
    ; :START and :END somehow get generated automatically even without being in *roles-to-emit* for extractions with IDs in the original LF, but not for extractions that have made up new IDs, e.g., MODALITY and some CCs
    :START
    :END
    #|
    ; from drumrules_misc
    :TIME-P  ; PERSISTENCE-VAL
    :TIME-V  ; SPEED-VAL
    :LEVEL2
    |#
    :ENTITY ; for "the amount of X"
    ))

;(setq *extraction-rules* '(drum))

(reset-im-rules 'drum)  ;; this allows you to edit this file and reload it without having to reload the entire system

(mapcar #'(lambda (x) (add-im-rule x 'drum))  ;; sets of rules are tagged so they can be managed independently 
	'(
	  
#|
;      (ONT::THE ONT::V68490 (:* ONT::FACILITY W::COMPLEX) :ASSOC-WITH
;       ONT::V68498 :START 0 :END 26 :WNSENSE "complex%1:06:00::")
;      (ONT::F ONT::V68498 (:* ONT::ASSOC-WITH W::OF) :VAL ONT::V68522 :OF
;       ONT::V68490 :START 12 :END 26)
;      (ONT::THE-SET ONT::V68522 ONT::MOLECULE :OPERATOR ONT::AND :SEQUENCE
;       (ONT::V68510 ONT::V68535) :START 15 :END 26)

; (test "the complex of RAS and RAF")

;;; complex
	  (
;	   ((? reln0 ONT::F ONT::THE ONT::BARE) ?ev 
;	    (:* (? type ONT::EVENT-OF-CHANGE) ?w) :agent ?!ag :affected ?!obj)
;	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag (? t1 ONT::CHEMICAL ONT::MOLECULAR-PART))
;	   ((? reln2 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj (? t2 ONT::CHEMICAL ONT::MOLECULAR-PART))
	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!complex (? t1 ONT::MOLECULAR-COMPLEX ONT::FACILITY) :ASSOC-WITH ?!c1)
	   (ONT::F ?!c1 (? tmp2 ONT::ASSOC-WITH) :VAL ?!c2)
	   (ONT::THE-SET ?!c2 (? tmp3 ONT::CHEMICAL ONT::MOLECULAR-PART) :OPERATOR ONT::AND :SEQUENCE (?!element1 ?!element2))
	   -complex1>
	   5
;	   (event ?ev ?type 
	   (event ?!complex ONT::BIND
	    :rule -complex1
	    :AFFECTED ?!element1
	    :AFFECTED1 ?!element2
            :TYPE ?type
	    ))

; need a second "complex" rule for this construction

;      (ONT::THE ONT::V68960 (:* ONT::FACILITY W::COMPLEX) :ASSOC-WITH
;       ONT::V68955 :START 0 :END 19 :WNSENSE "complex%1:06:00::")
;      (ONT::KIND ONT::V68955 (:* ONT::PROTEIN W::RAF) :ASSOC-WITH ONT::V68947
;       :NAME-OF ONT::RAF :START 0 :END 19 :WNSENSE "raf%1:14:01::" :DRUM
;       ((:DRUM :ID UP::Q06891 :NAME "Trans-acting factor D" :MATCHED-VARIANTS
;         ("RAF"))))
;      (ONT::THE ONT::V68947 (:* ONT::PROTEIN-FAMILY W::RAS) :NAME-OF (ONT::RAS)
;       :START 4 :END 8 :WNSENSE "ra%1:18:00::" :DRUM
;       ((:DRUM :ID XFAM::PF00071.17 :NAME "ras" :MATCHED-VARIANTS ("ras")))))
;     :UTTNUM 0)

|#

;;; modified rule to take into account term substitution (?w need to be removed because after TERM extraction, we get ":name Ras" slot instead)
;;; for both rules -activity1 and -activity2
	  ;; activity, e.g., "activity of Ras", "activity of Ras on Raf"
	  (((? reln ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev 
	    ;(:* ONT::ACTING W::ACTIVITY)
	     ONT::ACTIVITY-EVENT
	    :AGENT ?!name :AFFECTED ?obj)
	   (ONT::TERM ?!name 
;	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY) ?w))
; remove ?w and move :DRUM here 
	    (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY)) ; :DRUM ?code) 
	   (ONT::TERM ?obj 
;	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY) ?w))
; remove ?w and move :DRUM here 
	    (? type2 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY))
	   -activity1>
	   100
	   (ONT::event ?ev ONT::ACTIVITY
	    :rule -activity1
	    :AGENT ?!name
	    :AFFECTED ?obj
	    :type ONT::ACTIVITY-EVENT
;	    :drum ?code    ; removed :DRUM since now we have :AGENT 
	    )
	   )

#|
	  ;; this is temporary fix to remap AFFECTED to AGENT when no AGENT is present
	  ;; e.g. "Ras activity"
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ev 
	    (:* ONT::ACTING W::ACTIVITY)
	    :AGENT - :AFFECTED ?obj)
;	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?name 
;;	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY) ?w))
;; remove ?w and move :DRUM here 
;	    (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY)) ; :DRUM ?code) 
	   (TERM ?obj 
;	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY) ?w))
; remove ?w and move :DRUM here 
	    (? type2 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY))
	   -activity1a>
	   100
	   (event ?!ev ONT::ACTIVITY
	    :rule -activity1a
	    :AGENT ?obj
;	    :AGENT ?!name
;	    :AFFECTED ?obj
	    :type ONT::ACTING
;	    :drum ?code    ; removed :DRUM since now we have :AGENT 
	    )
	   )
|#

#|	  
	  ;; activity, e.g., "Ras activity"
	  ;; this rule doesn't fire any more (?)
	  ;; "Ras activity" is now parsed as ONT::ACTING :AFFECTED Ras, which is wrong (need to fix)
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    (:* ONT::ACTING W::ACTIVITY)
	    :ASSOC-WITHS (?!name))
	   (TERM ?!name 
	    (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY) :DRUM ?code)
	   -activity2>
	   100
	   (event ?!obj ONT::ACTIVITY
	    :rule -activity2
	    :AGENT ?!name
	    :type ONT::ACTING
	    :drum ?code
	    )
	   )
|#

;;; cannot handle multiple :ASSOC-WITH
;;; This is now also parsed as ONT::ACTING :AFFECTED phosphorylation
#|
	  ;; activity, e.g., "phosphorylation activity"
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    (:* ONT::ACTING W::ACTIVITY)
	    :ASSOC-WITH ?!name)
	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!name 
	    (:* (? type1 ONT::EVENT-OF-CHANGE ) ?w) :DRUM ?code)
	   -activity3>
	   100
	   (event ?!obj ONT::ACTIVITY
	    :rule -activity3
	    :activity ?!name
	    :type ONT::ACTING
	    )
	   )
|#

	  ;; activity, e.g., "transcriptional/phosphorylation activity"
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj 
	    (:* ONT::ACTIVITY-EVENT W::ACTIVITY)
	    :FIGURE ?!name)
	   (?reln1 ?!name 
	    (:* (? type1 ONT::EVENT-OF-CHANGE) ?w) :DRUM ?code) ; biological-process (e.g., transcriptional) is also included
	   -activity4>
	   100
	   (event ?!obj ?type1
	    :rule -activity4
	    :type ONT::ACTIVITY-EVENT
	    :drum ?code
	    )
	   )

	  ;; activity, e.g., "transcriptional/phosphorylation activity of Ras"
	  ;; Note: transcriptional is in the AFFECTED
	  ;; Note2: phosphorylation activity doesn't go through this rule?
	  (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ev
	    (:* ONT::ACTIVITY-EVENT W::ACTIVITY)
	    :AGENT ?!name :AFFECTED ?!obj)
	   (ONT::TERM ?!name
;	    (:* (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY) ?w))
; remove ?w and move :DRUM here 
	    (? type1 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::SIGNALING-PATHWAY))
	   (?reln2 ?!obj 
	    (? type2 ONT::EVENT-OF-CHANGE) :DRUM ?code) ; no ?w --- biological-process is already a TERM
	   -activity5>
	   100
	   (event ?!ev ?type2 
	    :rule -activity5
	    :agent ?!name
	    :type ONT::ACTIVITY-EVENT
	    :drum ?code
	    )
	   )

	  
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tyrosine phosphorylation of EGFR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::PHOSPHORYLATION ONT::UBIQUITINATION ONT::ACETYLATION ONT::FARNESYLATION ONT::GLYCOSYLATION ONT::HYDROXYLATION ONT::METHYLATION ONT::RIBOSYLATION ONT::SUMOYLATION ) ?!w) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq)
           (ONT::TERM ?!ag  (? t1 ONT::MOLECULAR-SITE ONT::MOLECULAR-DOMAIN ONT::AMINO-ACID ONT::RESIDUE ONT::TERMINUS))
           (ONT::TERM ?!obj (? t2 ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ)) ;ONT::SEQUENCE))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_AGENT_AFFECTED))
           -rule60_AGENT_AFFECTED-a>
;           60
           61
           (ONT::event ?ev ?!eventName
            :rule -rule60_AGENT_AFFECTED-a
;            :AGENT ?!ag
	    :AGENT -    ; zero out :AGENT
            :AFFECTED ?!obj
	    :SITE ?!ag
            :MODALITY ?modVal
	    :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
	  

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tyrosine phosphorylation at Y338 of EGFR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; (modified from rule60_AGENT_AFFECTED-LOC4 rule with only AFFECTED)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::PHOSPHORYLATION ONT::UBIQUITINATION ONT::ACETYLATION ONT::FARNESYLATION ONT::GLYCOSYLATION ONT::HYDROXYLATION ONT::METHYLATION ONT::RIBOSYLATION ONT::SUMOYLATION ) ?!w) :AFFECTED ?!obj :DRUM ?code :LOCATION ?!loc)
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::TERM ?!obj (? t2 ONT::MOLECULAR-SITE ONT::MOLECULAR-DOMAIN ONT::AMINO-ACID ONT::RESIDUE ONT::TERMINUS))
           (ONT::F ?!loc (? tmp ONT::SOURCE-RELN ONT::GOAL-RELN ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY) :VAL ?!locVal)
           (ONT::TERM ?!locVal (? locType ONT::MOLECULAR-SITE ONT::MOLECULAR-DOMAIN ONT::AMINO-ACID ONT::RESIDUE ONT::TERMINUS))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_AGENT_AFFECTED))
           -rule60_AGENT_AFFECTED-LOC4-AFFECTED-a>
;           60
           61
           (ONT::event ?ev ?!eventName
            :rule -rule60_AGENT_AFFECTED-LOC4-AFFECTED-a
;;            :AGENT ?!ag
;            :AFFECTED ?!obj
	    :AFFECTED -    ; zero out :AFFECTED
            :SITEMOD ?tmp ;?!loc
            :SITE ?!locVal
            :TYPE ?type
            :DRUM ?code
            )
           )

          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::PHOSPHORYLATION ONT::UBIQUITINATION ONT::ACETYLATION ONT::FARNESYLATION ONT::GLYCOSYLATION ONT::HYDROXYLATION ONT::METHYLATION ONT::RIBOSYLATION ONT::SUMOYLATION ) ?!w) :AFFECTED ?!obj :DRUM ?code :LOCATION ?!loc)
;           ((? reln1 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
           (ONT::TERM ?!obj (? t2 ONT::MOLECULAR-SITE ONT::MOLECULAR-DOMAIN ONT::AMINO-ACID ONT::RESIDUE ONT::TERMINUS))
           (ONT::F ?!loc (? tmp ONT::SOURCE-RELN ONT::GOAL-RELN ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY) :GROUND ?!locVal)
           (ONT::TERM ?!locVal (? locType ONT::MOLECULAR-SITE ONT::MOLECULAR-DOMAIN ONT::AMINO-ACID ONT::RESIDUE ONT::TERMINUS))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_AGENT_AFFECTED))
           -rule60_AGENT_AFFECTED-LOC4-AFFECTED-a-gd>
;           60
           61
           (ONT::event ?ev ?!eventName
            :rule -rule60_AGENT_AFFECTED-LOC4-AFFECTED-a-gd
;;            :AGENT ?!ag
;            :AFFECTED ?!obj
	    :AFFECTED -    ; zero out :AFFECTED
            :SITEMOD ?tmp ;?!loc
            :SITE ?!locVal
            :TYPE ?type
            :DRUM ?code
            )
           )
	  
	  

	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Generic rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Priority 3 (this matches EVENT-OF-CHANGE with AGENT and/or AFFECTED roles)
;;;;
;;	  ;; non-specific events w/ :agent and :affected roles
;;	  (((? reln ONT::F ONT::THE ONT::BARE) ?ev (:* (? type ONT::EVENT-OF-CHANGE) ?w) 
;;	    :agent ?!ag :affected ?!obj)
;;	   ((? reln0 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag ?agtype)
;;	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj ?objtype)
;;	   -generic-event-agent-affected>
;;	   3
;;	   (event ?ev ?type 
;;	    :rule -generic-event-agent-affected 
;;	    :agent ?!ag
;;	    :affected ?!obj
;;	    ))
;;
;;	  ;; non-specific events w/ :agent roles
;;	  (((? reln ONT::F ONT::THE ONT::BARE) ?ev (:* (? type ONT::EVENT-OF-CHANGE) ?w) 
;;	    :agent ?!ag)
;;	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag ?agtype)
;;	   -generic-event-agent>
;;	   3
;;	   (event ?ev ?type 
;;	    :rule -generic-event-agent 
;;	    :agent ?!ag
;;	    ))
;;
;;	  ;; non-specific events w/ :affected roles
;;	  (((? reln ONT::F ONT::THE ONT::BARE) ?ev (:* (? type ONT::EVENT-OF-CHANGE) ?w) 
;;	    :affected ?!obj)
;;	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj ?objtype)
;;	   -generic-event-affected>
;;	   3
;;	   (event ?ev ?type 
;;	    :rule -generic-event-affected 
;;	    :affected ?!obj
;;	    ))
;
;	  
;;;;;;;;;;;;;;;;;; Generic events with *both* AGENT and AFFECTED being CHEMICAL/MOLECULAR-PART (but not EVENT-OF-CHANGE) ;;;;;;;;;;;;;;;;;;;
;;; Priority 5
;;; 
;
;	  (((? reln0 ONT::F ONT::THE ONT::BARE) ?ev 
;	    (:* (? type ONT::EVENT-OF-CHANGE) ?w) :agent ?!ag :affected ?!obj)
;	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag (? t1 ONT::CHEMICAL ONT::MOLECULAR-PART))
;	   ((? reln2 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!obj (? t2 ONT::CHEMICAL ONT::MOLECULAR-PART))
;	   -generic-event-with-biological-entities>
;	   5
;;	   (event ?ev ?type 
;	   (event ?ev ONT::GENERIC
;	    :rule -generic-event-with-biological-entities 
;	    :agent ?!ag
;	    :affected ?!obj
;            :TYPE ?type
;	    ))


#|  moved to file for modality rules
;;;;;;;;;;
;; for some reason this rule doesn't work when put more towards the top, right after complex1.  Maybe complex1 doesn't work either?

	  ;; e.g., "we tested whether/if XYZ"
	  (((? reln ONT::F) ?!obj 
	    (? word W::CLAUSE-CONDITION) :CONTENT ?!cont
	    )
	   ((? reln1 ONT::F ONT::THE ONT::BARE) ?!cont 
	    ?t1)
	   -whether>
	   100
	   (modality ?!obj ONT::WHETHER
	    :event ?!cont
;	    :drum ?code
	    :rule -whether
	    )
	   )
|#

#|	  
;;;;;;;;; causal temporal extractions
; paper1
; check wither :DRUM is attached to the right LF clause
#|
; The above data indicate that, as a consequence of activation of RTKs, the Sos-1:Grb2 complex is disrupted, whereas the Sos-1:E3b1:Eps8 complex persists in the cell.

      (ONT::F ONT::V246389 (:* ONT::HINDERING W::DISRUPT) :AFFECTED
       ONT::V246364 :MOD ONT::V246224 :PROP + :TENSE ONT::PRES :PASSIVE +
       :START 24 :END 105)
      (ONT::F ONT::V246224 (:* ONT::AS-ROLE W::AS) :VAL ONT::V246278 :OF
       ONT::V246389 :START 30 :END 68 :WNSENSE "as%4:02:00::")
      (ONT::A ONT::V246278 (:* ONT::OUTCOME W::CONSEQUENCE) :OF ONT::V246288
       :START 33 :END 68 :WNSENSE "consequence%1:19:00::")
      (ONT::BARE ONT::V246288 (:* ONT::START W::ACTIVATION) :AFFECTED
       ONT::V246308 :START 47 :END 68 :WNSENSE "activation%1:22:00::")
|#
          (((? reln0 ONT::F ONT::THE ONT::BARE) ?!ev ?!t :MODS (?!v1) :DRUM ?code )
	   (ONT::F ?!v1  (:* ONT::AS-ROLE W::AS) :VAL ?!v2)
	   (ONT::A ?!v2 (:* ONT::OUTCOME W::CONSEQUENCE) :OF ?!v3)
           -causeA>
           40
           (event ?!ev ont::CAUSE  ; note: circular
            :rule -causeA
            :AGENT ?!v3
            :AFFECTED ?!ev
            :TYPE ONT::OUTCOME
            :DRUM ?code
            ))

;ERK can phosphorylate Sos-1 with ensuing reduced affinity for Grb2, but not for E3b1.
          (((? reln0 ONT::F ONT::THE ONT::BARE) ?!ev ?!t :MANNER ?!v1 :DRUM ?code )
	   (ONT::F ?!v1  (:* ONT::MANNER W::WITH) :VAL ?!v2)
	   (ONT::F ?!v3 ONT::OUTCOME-VAL :OF ?!v2)
           -causeB>
           40
           (event ?!ev ont::CAUSE  ; note: circular
            :rule -causeB
            :AGENT ?!ev
            :AFFECTED ?!v2
            :TYPE ONT::OUTCOME-VAL
            :DRUM ?code
            ))

; A Sos-1-E3b1 complex directs Rac activation by entering into a tricomplex with Eps8.
          (((? reln0 ONT::F ONT::THE ONT::BARE) ?!ev ?!t :AGENT ?!ag :BY-MEANS-OF ?!v1 :DRUM ?code )
	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!ag  (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ONT::MUTANT-OBJ ONT::WILDTYPE-OBJ ONT::SEQUENCE))
	   (ONT::F ?!v1 (:* ONT::BY-MEANS-OF ?w1) :VAL ?!v2)
	   (ONT::F ?!v2 (:* ONT::ENROLL ?w2) :RESULT ?!v3 :MANNER ?!v4)
	   ((? reln4 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!v3 ONT::MACROMOLECULAR-COMPLEX)
	   (ONT::F ?!v4 (:* ONT::MANNER W::WITH) :VAL ?!v5)
           -bmo>
           40
           (event ?!v2 ont::BIND
            :rule -bmo
            :AGENT ?!ag
            :AFFECTED ?!v5
            :TYPE ONT::ENROLL
            :DRUM ?code
            ))


; The ability of E3b1 to form a complex with Eps8 is required to transmit signals from Ras to Rac.
          (((? reln0 ONT::F ONT::THE ONT::BARE) ?!ev (:* (? t ONT::SEND) ?!w) 
	    :AFFECTED ?!aff :SOURCE ?!src :RESULT ?!res :DRUM ?code )
	   (ONT::BARE ?!aff (:* ONT::COMMUNICATION ?!w2))
	   (ONT::F ?!src (:* ONT::FROM-LOC ?!w3) :VAL ?!srcV)
	   (ONT::BARE ?!srcV 
	    (? t2 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ))
	   (ONT::F ?!res (:* ONT::TO-LOC ?!w4) :VAL ?!resV)
	   (ONT::BARE ?!resV 
	    (? t3 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ))
           -regulateA>
           40
           (event ?!ev ont::REGULATE
            :rule -regulateA
            :AGENT ?!srcV
            :AFFECTED ?!resV
            :TYPE ?t
            :DRUM ?code
            ))

; RTK-induced Rac activation is apparently Ras independent.
          (((? reln0 ONT::F ONT::THE ONT::BARE) ?!ev (:* (? t ONT::INDEPENDENT) ?!w) :OF ?!v1 :VAL ?!v2 :DRUM ?code )
           -indep>
           40
           (event ?ev ont::INDEPENDENT
            :rule -indep
            :NEUTRAL ?!v1
            :NEUTRAL1 ?!v2
            :TYPE ?t
            :DRUM ?code   ; probably no drum info
            ))

#|
; Activation of Ras was rapid and short lived, whereas activation of Rac was sustained over a longer period of time.
          (((? reln0 ONT::F ONT::THE ONT::BARE) ?ev (:* (? t ONT::HAVE-PROPERTY) ?w) :NEUTRAL ?!v1 :FORMAL ?!v2 :DRUM ?code )
	   ((? reln1 ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?!v1 
	    (? t1 ONT::EVENT-OF-CHANGE ONT::CHEMICAL ONT::MOLECULAR-PART ONT::CELL-PART ONT::BODY-PART ONT::SIGNALING-PATHWAY ))
           -be>
           40
           (event ?ev ONT::BE
            :rule -be
            :NEUTRAL ?!v1
            :FORMAL ?!v2
            :TYPE ?t
            :DRUM ?code
            ))
|#
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; START: rules automatically generated from template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_AFFECTED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_AFFECTED rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-AFFECTED>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_AFFECTED rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-robust-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-robust-AFFECTED-TERM
            ))
          ;; rule40_4_AGENT_AFFECTED rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-robustProSELF-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-robustPro-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-robustPro-AGENT-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-robustPro-AFFECTED-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_AFFECTED-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_AFFECTED-POLARITY1-gd rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-POLARITY1-gd>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-POLARITY1-gd
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-POLARITY1-gd rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-POLARITY1-gd-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-POLARITY1-gd-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-POLARITY1-gd rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-POLARITY1-gd-AFFECTED>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-POLARITY1-gd-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-POLARITY1-gd rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_AFFECTED-POLARITY1-gd rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robust-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robust-AFFECTED-TERM
            ))
          ;; rule40_4_AGENT_AFFECTED-POLARITY1-gd rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-POLARITY1-gd-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-POLARITY1-gd-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-POLARITY1-gd rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robustProSELF-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-POLARITY1-gd rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-POLARITY1-gd rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-POLARITY1-gd rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-POLARITY1-gd rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AFFECTED-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-POLARITY1-gd rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_AFFECTED-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_AFFECTED-MODA1_1 rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_1>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_1
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-MODA1_1 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_1-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_1-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODA1_1 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_1-AFFECTED>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_1-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-MODA1_1 rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_1-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_1-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-MODA1_1-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_AFFECTED-MODA1_1 rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_1-robust-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_1-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-MODA1_1-robust-AFFECTED-TERM
            ))
          ;; rule40_4_AGENT_AFFECTED-MODA1_1 rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_1-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_1-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODA1_1 rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_1-robustProSELF-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_1-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODA1_1 rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_1-robustPro-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_1-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODA1_1 rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODA1_1 rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODA1_1 rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_1-robustPro-AFFECTED-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_1-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODA1_1 rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_AFFECTED-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_AFFECTED-MODA1_2 rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_2>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_2
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-MODA1_2 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_2-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_2-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODA1_2 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_2-AFFECTED>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_2-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-MODA1_2 rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_2-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_2-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-MODA1_2-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_AFFECTED-MODA1_2 rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_2-robust-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_2-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-MODA1_2-robust-AFFECTED-TERM
            ))
          ;; rule40_4_AGENT_AFFECTED-MODA1_2 rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_2-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_2-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODA1_2 rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_2-robustProSELF-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_2-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODA1_2 rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_2-robustPro-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_2-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODA1_2 rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODA1_2 rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODA1_2 rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_2-robustPro-AFFECTED-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_2-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODA1_2 rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_AFFECTED-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_AFFECTED-MODN1_1 rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_1>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_1
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-MODN1_1 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_1-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_1-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODN1_1 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_1-AFFECTED>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_1-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-MODN1_1 rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_1-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_1-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-MODN1_1-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_AFFECTED-MODN1_1 rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_1-robust-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_1-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-MODN1_1-robust-AFFECTED-TERM
            ))
          ;; rule40_4_AGENT_AFFECTED-MODN1_1 rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_1-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_1-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODN1_1 rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_1-robustProSELF-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_1-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODN1_1 rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_1-robustPro-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_1-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODN1_1 rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODN1_1 rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODN1_1 rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_1-robustPro-AFFECTED-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_1-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODN1_1 rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_AFFECTED-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_AFFECTED-MODN1_2 rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_2>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_2
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-MODN1_2 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_2-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_2-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODN1_2 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_2-AFFECTED>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_2-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-MODN1_2 rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_2-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_2-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-MODN1_2-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_AFFECTED-MODN1_2 rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_2-robust-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_2-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-MODN1_2-robust-AFFECTED-TERM
            ))
          ;; rule40_4_AGENT_AFFECTED-MODN1_2 rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_2-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_2-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODN1_2 rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_2-robustProSELF-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_2-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODN1_2 rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_2-robustPro-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_2-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODN1_2 rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODN1_2 rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODN1_2 rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_2-robustPro-AFFECTED-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_2-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-MODN1_2 rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ONT::APPEAR ONT::STOP ONT::CONSUME ONT::TAKE-IN ONT::BREAK-OBJECT ONT::SEPARATION ONT::RENDER-INEFFECTIVE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ONT::CHANGE-INTEGRITY ONT::DIE ONT::DAMAGE ONT::DESTROY ONT::EXPLODE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_AFFECTED-RESULT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_AFFECTED-RESULT rule with AGENT and AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-AGENT
            :AGENT ?!ag
            :AFFECTED-RESULT -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT rule with only AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-AFFECTED-RESULT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-AFFECTED-RESULT
            :AGENT -
            :AFFECTED-RESULT ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT rule with known AFFECTED-RESULT but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-robust-AGENT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-RESULT-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT rule with known AGENT but unknown proper name argument as AFFECTED-RESULT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-robust-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-robust-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-RESULT-robust-AFFECTED-RESULT-TERM
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT rule with neither AGENT nor AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT - :AFFECTED-RESULT - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-none
;            :AGENT ?!ag
;            :AFFECTED-RESULT ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT rule with known AGENT but a reflexive pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-robustProSELF-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-robustProSELF-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT rule with known AGENT but a pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-robustPro-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-robustPro-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT rule with a pronoun as AGENT but a known AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-robustPro-AGENT
            :AFFECTED-RESULT ?!obj
            :AGENT ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT rule with pronouns as both AGENT and AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-robustPro-AGENT-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-robustPro-AGENT-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT rule with no AGENT and a pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-robustPro-AFFECTED-RESULT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-robustPro-AFFECTED-RESULT-only
            :AGENT -
            :AFFECTED-RESULT ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT rule with a pronoun as AGENT and no AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED-RESULT -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd rule with AGENT and AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-AGENT
            :AGENT ?!ag
            :AFFECTED-RESULT -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd rule with only AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-AFFECTED-RESULT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-AFFECTED-RESULT
            :AGENT -
            :AFFECTED-RESULT ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd rule with known AFFECTED-RESULT but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robust-AGENT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd rule with known AGENT but unknown proper name argument as AFFECTED-RESULT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robust-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robust-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robust-AFFECTED-RESULT-TERM
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd rule with neither AGENT nor AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT - :AFFECTED-RESULT - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-none
;            :AGENT ?!ag
;            :AFFECTED-RESULT ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd rule with known AGENT but a reflexive pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robustProSELF-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robustProSELF-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd rule with known AGENT but a pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robustPro-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robustPro-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd rule with a pronoun as AGENT but a known AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robustPro-AGENT
            :AFFECTED-RESULT ?!obj
            :AGENT ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd rule with pronouns as both AGENT and AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robustPro-AGENT-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robustPro-AGENT-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd rule with no AGENT and a pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robustPro-AFFECTED-RESULT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robustPro-AFFECTED-RESULT-only
            :AGENT -
            :AFFECTED-RESULT ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd rule with a pronoun as AGENT and no AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-POLARITY1-gd-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED-RESULT -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_1 rule with AGENT and AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_1 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-AGENT
            :AGENT ?!ag
            :AFFECTED-RESULT -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_1 rule with only AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-AFFECTED-RESULT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-AFFECTED-RESULT
            :AGENT -
            :AFFECTED-RESULT ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_1 rule with known AFFECTED-RESULT but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robust-AGENT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_1 rule with known AGENT but unknown proper name argument as AFFECTED-RESULT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robust-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robust-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robust-AFFECTED-RESULT-TERM
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_1 rule with neither AGENT nor AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT - :AFFECTED-RESULT - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-none
;            :AGENT ?!ag
;            :AFFECTED-RESULT ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_1 rule with known AGENT but a reflexive pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robustProSELF-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robustProSELF-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_1 rule with known AGENT but a pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robustPro-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robustPro-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_1 rule with a pronoun as AGENT but a known AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robustPro-AGENT
            :AFFECTED-RESULT ?!obj
            :AGENT ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_1 rule with pronouns as both AGENT and AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robustPro-AGENT-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robustPro-AGENT-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_1 rule with no AGENT and a pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robustPro-AFFECTED-RESULT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robustPro-AFFECTED-RESULT-only
            :AGENT -
            :AFFECTED-RESULT ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_1 rule with a pronoun as AGENT and no AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_1-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED-RESULT -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_2 rule with AGENT and AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_2 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-AGENT
            :AGENT ?!ag
            :AFFECTED-RESULT -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_2 rule with only AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-AFFECTED-RESULT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-AFFECTED-RESULT
            :AGENT -
            :AFFECTED-RESULT ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_2 rule with known AFFECTED-RESULT but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robust-AGENT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_2 rule with known AGENT but unknown proper name argument as AFFECTED-RESULT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robust-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robust-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robust-AFFECTED-RESULT-TERM
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_2 rule with neither AGENT nor AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT - :AFFECTED-RESULT - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-none
;            :AGENT ?!ag
;            :AFFECTED-RESULT ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_2 rule with known AGENT but a reflexive pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robustProSELF-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robustProSELF-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_2 rule with known AGENT but a pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robustPro-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robustPro-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_2 rule with a pronoun as AGENT but a known AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robustPro-AGENT
            :AFFECTED-RESULT ?!obj
            :AGENT ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_2 rule with pronouns as both AGENT and AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robustPro-AGENT-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robustPro-AGENT-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_2 rule with no AGENT and a pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robustPro-AFFECTED-RESULT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robustPro-AFFECTED-RESULT-only
            :AGENT -
            :AFFECTED-RESULT ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODA1_2 rule with a pronoun as AGENT and no AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODA1_2-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED-RESULT -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_1 rule with AGENT and AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_1 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-AGENT
            :AGENT ?!ag
            :AFFECTED-RESULT -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_1 rule with only AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-AFFECTED-RESULT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-AFFECTED-RESULT
            :AGENT -
            :AFFECTED-RESULT ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_1 rule with known AFFECTED-RESULT but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robust-AGENT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_1 rule with known AGENT but unknown proper name argument as AFFECTED-RESULT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robust-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robust-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robust-AFFECTED-RESULT-TERM
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_1 rule with neither AGENT nor AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT - :AFFECTED-RESULT - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-none
;            :AGENT ?!ag
;            :AFFECTED-RESULT ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_1 rule with known AGENT but a reflexive pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robustProSELF-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robustProSELF-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_1 rule with known AGENT but a pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robustPro-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robustPro-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_1 rule with a pronoun as AGENT but a known AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robustPro-AGENT
            :AFFECTED-RESULT ?!obj
            :AGENT ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_1 rule with pronouns as both AGENT and AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robustPro-AGENT-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robustPro-AGENT-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_1 rule with no AGENT and a pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robustPro-AFFECTED-RESULT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robustPro-AFFECTED-RESULT-only
            :AGENT -
            :AFFECTED-RESULT ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_1 rule with a pronoun as AGENT and no AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_1-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED-RESULT -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_2 rule with AGENT and AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_2 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-AGENT
            :AGENT ?!ag
            :AFFECTED-RESULT -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_2 rule with only AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-AFFECTED-RESULT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-AFFECTED-RESULT
            :AGENT -
            :AFFECTED-RESULT ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_2 rule with known AFFECTED-RESULT but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robust-AGENT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_2 rule with known AGENT but unknown proper name argument as AFFECTED-RESULT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robust-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robust-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robust-AFFECTED-RESULT-TERM
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_2 rule with neither AGENT nor AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT - :AFFECTED-RESULT - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-none
;            :AGENT ?!ag
;            :AFFECTED-RESULT ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_2 rule with known AGENT but a reflexive pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robustProSELF-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robustProSELF-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_2 rule with known AGENT but a pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robustPro-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robustPro-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_2 rule with a pronoun as AGENT but a known AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robustPro-AGENT
            :AFFECTED-RESULT ?!obj
            :AGENT ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_2 rule with pronouns as both AGENT and AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robustPro-AGENT-AFFECTED-RESULT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robustPro-AGENT-AFFECTED-RESULT
            :AGENT ?!ag
            :AFFECTED-RESULT ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_2 rule with no AGENT and a pronoun as AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AFFECTED-RESULT ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robustPro-AFFECTED-RESULT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robustPro-AFFECTED-RESULT-only
            :AGENT -
            :AFFECTED-RESULT ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_AFFECTED-RESULT-MODN1_2 rule with a pronoun as AGENT and no AFFECTED-RESULT
          ((?!reln0 ?ev
            (? type ONT::CREATE ONT::CAUSE-PRODUCE-REPRODUCE ONT::BE-BORN ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED-RESULT))
           -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED-RESULT-MODN1_2-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED-RESULT -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AFFECTED_AFFECTED1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AFFECTED_AFFECTED1 rule with AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-AFFECTED>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1 rule with only AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-AFFECTED1>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-AFFECTED1
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1 rule with known AFFECTED1 but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-robust-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-robust-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AFFECTED_AFFECTED1-robust-AFFECTED-TERM
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1 rule with known AFFECTED but unknown proper name argument as AFFECTED1
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-robust-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-robust-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AFFECTED_AFFECTED1-robust-AFFECTED1-TERM
            ))
          ;; rule40_4_AFFECTED_AFFECTED1 rule with neither AFFECTED nor AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED - :AFFECTED1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-none
;            :AFFECTED ?!ag
;            :AFFECTED1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1 rule with known AFFECTED but a reflexive pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-robustProSELF-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-robustProSELF-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1 rule with known AFFECTED but a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-robustPro-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-robustPro-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1 rule with a pronoun as AFFECTED but a known AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-robustPro-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-robustPro-AFFECTED
            :AFFECTED1 ?!obj
            :AFFECTED ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1 rule with pronouns as both AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-robustPro-AFFECTED-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-robustPro-AFFECTED-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1 rule with no AFFECTED and a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-robustPro-AFFECTED1-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-robustPro-AFFECTED1-only
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1 rule with a pronoun as AFFECTED and no AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-robustPro-AFFECTED-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-robustPro-AFFECTED-only
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-AFFECTED>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with only AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-AFFECTED1>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-AFFECTED1
            :AFFECTED -
            :AFFECTED1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with known AFFECTED1 but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robust-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robust-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robust-AFFECTED-TERM
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with known AFFECTED but unknown proper name argument as AFFECTED1
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robust-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robust-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robust-AFFECTED1-TERM
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with neither AFFECTED nor AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED - :AFFECTED1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-none
;            :AFFECTED ?!ag
;            :AFFECTED1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with known AFFECTED but a reflexive pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustProSELF-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustProSELF-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with known AFFECTED but a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with a pronoun as AFFECTED but a known AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED
            :AFFECTED1 ?!obj
            :AFFECTED ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with pronouns as both AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with no AFFECTED and a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED1-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED1-only
            :AFFECTED -
            :AFFECTED1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with a pronoun as AFFECTED and no AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED-only
            :AFFECTED ?!ag
            :AFFECTED1 -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AFFECTED_AFFECTED1-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_1 rule with AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_1>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_1 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_1-AFFECTED>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_1-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_1 rule with only AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_1-AFFECTED1>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_1-AFFECTED1
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_1 rule with known AFFECTED1 but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robust-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robust-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robust-AFFECTED-TERM
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_1 rule with known AFFECTED but unknown proper name argument as AFFECTED1
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robust-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robust-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robust-AFFECTED1-TERM
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_1 rule with neither AFFECTED nor AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED - :AFFECTED1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_1-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_1-none
;            :AFFECTED ?!ag
;            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_1 rule with known AFFECTED but a reflexive pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robustProSELF-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robustProSELF-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_1 rule with known AFFECTED but a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_1 rule with a pronoun as AFFECTED but a known AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED
            :AFFECTED1 ?!obj
            :AFFECTED ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_1 rule with pronouns as both AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_1 rule with no AFFECTED and a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED1-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED1-only
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_1 rule with a pronoun as AFFECTED and no AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED-only
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AFFECTED_AFFECTED1-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_2 rule with AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_2>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_2
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_2 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_2-AFFECTED>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_2-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_2 rule with only AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_2-AFFECTED1>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_2-AFFECTED1
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_2 rule with known AFFECTED1 but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robust-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robust-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robust-AFFECTED-TERM
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_2 rule with known AFFECTED but unknown proper name argument as AFFECTED1
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robust-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robust-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robust-AFFECTED1-TERM
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_2 rule with neither AFFECTED nor AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED - :AFFECTED1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_2-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_2-none
;            :AFFECTED ?!ag
;            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_2 rule with known AFFECTED but a reflexive pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robustProSELF-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robustProSELF-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_2 rule with known AFFECTED but a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_2 rule with a pronoun as AFFECTED but a known AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED
            :AFFECTED1 ?!obj
            :AFFECTED ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_2 rule with pronouns as both AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_2 rule with no AFFECTED and a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED1-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED1-only
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODA1_2 rule with a pronoun as AFFECTED and no AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED-only
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AFFECTED_AFFECTED1-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_1 rule with AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_1>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_1 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_1-AFFECTED>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_1-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_1 rule with only AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_1-AFFECTED1>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_1-AFFECTED1
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_1 rule with known AFFECTED1 but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robust-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robust-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robust-AFFECTED-TERM
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_1 rule with known AFFECTED but unknown proper name argument as AFFECTED1
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robust-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robust-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robust-AFFECTED1-TERM
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_1 rule with neither AFFECTED nor AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED - :AFFECTED1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_1-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_1-none
;            :AFFECTED ?!ag
;            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_1 rule with known AFFECTED but a reflexive pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robustProSELF-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robustProSELF-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_1 rule with known AFFECTED but a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_1 rule with a pronoun as AFFECTED but a known AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED
            :AFFECTED1 ?!obj
            :AFFECTED ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_1 rule with pronouns as both AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_1 rule with no AFFECTED and a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED1-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED1-only
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_1 rule with a pronoun as AFFECTED and no AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED-only
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AFFECTED_AFFECTED1-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_2 rule with AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_2>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_2
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_2 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_2-AFFECTED>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_2-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_2 rule with only AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_2-AFFECTED1>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_2-AFFECTED1
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_2 rule with known AFFECTED1 but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robust-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robust-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robust-AFFECTED-TERM
            )
           )
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_2 rule with known AFFECTED but unknown proper name argument as AFFECTED1
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robust-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robust-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robust-AFFECTED1-TERM
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_2 rule with neither AFFECTED nor AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED - :AFFECTED1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_2-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_2-none
;            :AFFECTED ?!ag
;            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_2 rule with known AFFECTED but a reflexive pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robustProSELF-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robustProSELF-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_2 rule with known AFFECTED but a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_2 rule with a pronoun as AFFECTED but a known AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED
            :AFFECTED1 ?!obj
            :AFFECTED ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_2 rule with pronouns as both AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED-AFFECTED1>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_2 rule with no AFFECTED and a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED1-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED1-only
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AFFECTED_AFFECTED1-MODN1_2 rule with a pronoun as AFFECTED and no AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::BREAK-OBJECT ONT::SEPARATION ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AFFECTED_AFFECTED1))
           -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED-only
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_FORMAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_FORMAL rule with AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_FORMAL rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-AGENT
            :AGENT ?!ag
            :FORMAL -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL rule with only FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-FORMAL>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_FORMAL rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_FORMAL-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_FORMAL rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-robust-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_FORMAL-robust-FORMAL-TERM
            ))
          ;; rule40_4_AGENT_FORMAL rule with neither AGENT nor FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL rule with known AGENT but a reflexive pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-robustProSELF-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL rule with known AGENT but a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-robustPro-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL rule with a pronoun as AGENT but a known FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-robustPro-AGENT
            :FORMAL ?!obj
            :AGENT ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL rule with pronouns as both AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-robustPro-AGENT-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-robustPro-AGENT-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL rule with no AGENT and a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-robustPro-FORMAL-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-robustPro-FORMAL-only
            :AGENT -
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL rule with a pronoun as AGENT and no FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-robustPro-AGENT-only
            :AGENT ?!ag
            :FORMAL -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_FORMAL-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_FORMAL-POLARITY1-gd rule with AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-POLARITY1-gd>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-POLARITY1-gd
            :AGENT ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_FORMAL-POLARITY1-gd rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-POLARITY1-gd-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-POLARITY1-gd-AGENT
            :AGENT ?!ag
            :FORMAL -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-POLARITY1-gd rule with only FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-POLARITY1-gd-FORMAL>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-POLARITY1-gd-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_FORMAL-POLARITY1-gd rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-POLARITY1-gd-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-POLARITY1-gd-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_FORMAL-POLARITY1-gd-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_FORMAL-POLARITY1-gd rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-POLARITY1-gd-robust-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-POLARITY1-gd-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_FORMAL-POLARITY1-gd-robust-FORMAL-TERM
            ))
          ;; rule40_4_AGENT_FORMAL-POLARITY1-gd rule with neither AGENT nor FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-POLARITY1-gd-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-POLARITY1-gd-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-POLARITY1-gd rule with known AGENT but a reflexive pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-POLARITY1-gd-robustProSELF-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-POLARITY1-gd-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-POLARITY1-gd rule with known AGENT but a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-POLARITY1-gd-robustPro-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-POLARITY1-gd-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-POLARITY1-gd rule with a pronoun as AGENT but a known FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-POLARITY1-gd-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-POLARITY1-gd-robustPro-AGENT
            :FORMAL ?!obj
            :AGENT ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-POLARITY1-gd rule with pronouns as both AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-POLARITY1-gd-robustPro-AGENT-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-POLARITY1-gd-robustPro-AGENT-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-POLARITY1-gd rule with no AGENT and a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-POLARITY1-gd-robustPro-FORMAL-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-POLARITY1-gd-robustPro-FORMAL-only
            :AGENT -
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-POLARITY1-gd rule with a pronoun as AGENT and no FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-POLARITY1-gd-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-POLARITY1-gd-robustPro-AGENT-only
            :AGENT ?!ag
            :FORMAL -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_FORMAL-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_FORMAL-MODA1_1 rule with AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_1>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_1
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_FORMAL-MODA1_1 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_1-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_1-AGENT
            :AGENT ?!ag
            :FORMAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODA1_1 rule with only FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_1-FORMAL>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_1-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_FORMAL-MODA1_1 rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_1-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_1-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_FORMAL-MODA1_1-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_FORMAL-MODA1_1 rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_1-robust-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_1-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_FORMAL-MODA1_1-robust-FORMAL-TERM
            ))
          ;; rule40_4_AGENT_FORMAL-MODA1_1 rule with neither AGENT nor FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_1-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_1-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODA1_1 rule with known AGENT but a reflexive pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_1-robustProSELF-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_1-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODA1_1 rule with known AGENT but a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_1-robustPro-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_1-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODA1_1 rule with a pronoun as AGENT but a known FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_1-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_1-robustPro-AGENT
            :FORMAL ?!obj
            :AGENT ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODA1_1 rule with pronouns as both AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_1-robustPro-AGENT-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_1-robustPro-AGENT-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODA1_1 rule with no AGENT and a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_1-robustPro-FORMAL-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_1-robustPro-FORMAL-only
            :AGENT -
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODA1_1 rule with a pronoun as AGENT and no FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_1-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_1-robustPro-AGENT-only
            :AGENT ?!ag
            :FORMAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_FORMAL-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_FORMAL-MODA1_2 rule with AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_2>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_2
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_FORMAL-MODA1_2 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_2-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_2-AGENT
            :AGENT ?!ag
            :FORMAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODA1_2 rule with only FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_2-FORMAL>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_2-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_FORMAL-MODA1_2 rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_2-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_2-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_FORMAL-MODA1_2-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_FORMAL-MODA1_2 rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_2-robust-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_2-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_FORMAL-MODA1_2-robust-FORMAL-TERM
            ))
          ;; rule40_4_AGENT_FORMAL-MODA1_2 rule with neither AGENT nor FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_2-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_2-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODA1_2 rule with known AGENT but a reflexive pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_2-robustProSELF-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_2-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODA1_2 rule with known AGENT but a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_2-robustPro-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_2-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODA1_2 rule with a pronoun as AGENT but a known FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_2-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_2-robustPro-AGENT
            :FORMAL ?!obj
            :AGENT ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODA1_2 rule with pronouns as both AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_2-robustPro-AGENT-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_2-robustPro-AGENT-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODA1_2 rule with no AGENT and a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_2-robustPro-FORMAL-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_2-robustPro-FORMAL-only
            :AGENT -
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODA1_2 rule with a pronoun as AGENT and no FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODA1_2-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODA1_2-robustPro-AGENT-only
            :AGENT ?!ag
            :FORMAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_FORMAL-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_FORMAL-MODN1_1 rule with AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_1>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_1
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_FORMAL-MODN1_1 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_1-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_1-AGENT
            :AGENT ?!ag
            :FORMAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODN1_1 rule with only FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_1-FORMAL>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_1-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_FORMAL-MODN1_1 rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_1-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_1-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_FORMAL-MODN1_1-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_FORMAL-MODN1_1 rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_1-robust-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_1-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_FORMAL-MODN1_1-robust-FORMAL-TERM
            ))
          ;; rule40_4_AGENT_FORMAL-MODN1_1 rule with neither AGENT nor FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_1-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_1-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODN1_1 rule with known AGENT but a reflexive pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_1-robustProSELF-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_1-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODN1_1 rule with known AGENT but a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_1-robustPro-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_1-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODN1_1 rule with a pronoun as AGENT but a known FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_1-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_1-robustPro-AGENT
            :FORMAL ?!obj
            :AGENT ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODN1_1 rule with pronouns as both AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_1-robustPro-AGENT-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_1-robustPro-AGENT-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODN1_1 rule with no AGENT and a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_1-robustPro-FORMAL-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_1-robustPro-FORMAL-only
            :AGENT -
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODN1_1 rule with a pronoun as AGENT and no FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_1-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_1-robustPro-AGENT-only
            :AGENT ?!ag
            :FORMAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule40_4_AGENT_FORMAL-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule40_4_AGENT_FORMAL-MODN1_2 rule with AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_2>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_2
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_FORMAL-MODN1_2 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_2-AGENT>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_2-AGENT
            :AGENT ?!ag
            :FORMAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODN1_2 rule with only FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_2-FORMAL>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_2-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule40_4_AGENT_FORMAL-MODN1_2 rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_2-robust-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_2-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule40_4_AGENT_FORMAL-MODN1_2-robust-AGENT-TERM
            )
           )
          ;; rule40_4_AGENT_FORMAL-MODN1_2 rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_2-robust-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_2-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule40_4_AGENT_FORMAL-MODN1_2-robust-FORMAL-TERM
            ))
          ;; rule40_4_AGENT_FORMAL-MODN1_2 rule with neither AGENT nor FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_2-none>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_2-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODN1_2 rule with known AGENT but a reflexive pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_2-robustProSELF-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_2-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODN1_2 rule with known AGENT but a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_2-robustPro-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_2-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODN1_2 rule with a pronoun as AGENT but a known FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_2-robustPro-AGENT>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_2-robustPro-AGENT
            :FORMAL ?!obj
            :AGENT ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODN1_2 rule with pronouns as both AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_2-robustPro-AGENT-FORMAL>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_2-robustPro-AGENT-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODN1_2 rule with no AGENT and a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_2-robustPro-FORMAL-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_2-robustPro-FORMAL-only
            :AGENT -
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule40_4_AGENT_FORMAL-MODN1_2 rule with a pronoun as AGENT and no FORMAL
          ((?!reln0 ?ev
            (? type ONT::RENDER-INEFFECTIVE ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_FORMAL))
           -rule40_4_AGENT_FORMAL-MODN1_2-robustPro-AGENT-only>
           39
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_FORMAL-MODN1_2-robustPro-AGENT-only
            :AGENT ?!ag
            :FORMAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule30_4_AGENT_FORMAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule30_4_AGENT_FORMAL rule with AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule30_4_AGENT_FORMAL rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-AGENT>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-AGENT
            :AGENT ?!ag
            :FORMAL -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL rule with only FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-FORMAL>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule30_4_AGENT_FORMAL rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-robust-AGENT>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule30_4_AGENT_FORMAL-robust-AGENT-TERM
            )
           )
          ;; rule30_4_AGENT_FORMAL rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-robust-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule30_4_AGENT_FORMAL-robust-FORMAL-TERM
            ))
          ;; rule30_4_AGENT_FORMAL rule with neither AGENT nor FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-none>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL rule with known AGENT but a reflexive pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-robustProSELF-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL rule with known AGENT but a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-robustPro-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL rule with a pronoun as AGENT but a known FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-robustPro-AGENT>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-robustPro-AGENT
            :FORMAL ?!obj
            :AGENT ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL rule with pronouns as both AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-robustPro-AGENT-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-robustPro-AGENT-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL rule with no AGENT and a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-robustPro-FORMAL-only>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-robustPro-FORMAL-only
            :AGENT -
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL rule with a pronoun as AGENT and no FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-robustPro-AGENT-only>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-robustPro-AGENT-only
            :AGENT ?!ag
            :FORMAL -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule30_4_AGENT_FORMAL-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule30_4_AGENT_FORMAL-POLARITY1-gd rule with AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-POLARITY1-gd>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-POLARITY1-gd
            :AGENT ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule30_4_AGENT_FORMAL-POLARITY1-gd rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-POLARITY1-gd-AGENT>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-POLARITY1-gd-AGENT
            :AGENT ?!ag
            :FORMAL -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-POLARITY1-gd rule with only FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-POLARITY1-gd-FORMAL>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-POLARITY1-gd-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule30_4_AGENT_FORMAL-POLARITY1-gd rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-POLARITY1-gd-robust-AGENT>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-POLARITY1-gd-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule30_4_AGENT_FORMAL-POLARITY1-gd-robust-AGENT-TERM
            )
           )
          ;; rule30_4_AGENT_FORMAL-POLARITY1-gd rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-POLARITY1-gd-robust-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-POLARITY1-gd-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule30_4_AGENT_FORMAL-POLARITY1-gd-robust-FORMAL-TERM
            ))
          ;; rule30_4_AGENT_FORMAL-POLARITY1-gd rule with neither AGENT nor FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-POLARITY1-gd-none>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-POLARITY1-gd-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-POLARITY1-gd rule with known AGENT but a reflexive pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-POLARITY1-gd-robustProSELF-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-POLARITY1-gd-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-POLARITY1-gd rule with known AGENT but a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-POLARITY1-gd-robustPro-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-POLARITY1-gd-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-POLARITY1-gd rule with a pronoun as AGENT but a known FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-POLARITY1-gd-robustPro-AGENT>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-POLARITY1-gd-robustPro-AGENT
            :FORMAL ?!obj
            :AGENT ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-POLARITY1-gd rule with pronouns as both AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-POLARITY1-gd-robustPro-AGENT-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-POLARITY1-gd-robustPro-AGENT-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-POLARITY1-gd rule with no AGENT and a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-POLARITY1-gd-robustPro-FORMAL-only>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-POLARITY1-gd-robustPro-FORMAL-only
            :AGENT -
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-POLARITY1-gd rule with a pronoun as AGENT and no FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-POLARITY1-gd-robustPro-AGENT-only>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-POLARITY1-gd-robustPro-AGENT-only
            :AGENT ?!ag
            :FORMAL -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule30_4_AGENT_FORMAL-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule30_4_AGENT_FORMAL-MODA1_1 rule with AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_1>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_1
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule30_4_AGENT_FORMAL-MODA1_1 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_1-AGENT>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_1-AGENT
            :AGENT ?!ag
            :FORMAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODA1_1 rule with only FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_1-FORMAL>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_1-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule30_4_AGENT_FORMAL-MODA1_1 rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_1-robust-AGENT>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_1-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule30_4_AGENT_FORMAL-MODA1_1-robust-AGENT-TERM
            )
           )
          ;; rule30_4_AGENT_FORMAL-MODA1_1 rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_1-robust-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_1-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule30_4_AGENT_FORMAL-MODA1_1-robust-FORMAL-TERM
            ))
          ;; rule30_4_AGENT_FORMAL-MODA1_1 rule with neither AGENT nor FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_1-none>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_1-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODA1_1 rule with known AGENT but a reflexive pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_1-robustProSELF-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_1-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODA1_1 rule with known AGENT but a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_1-robustPro-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_1-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODA1_1 rule with a pronoun as AGENT but a known FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_1-robustPro-AGENT>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_1-robustPro-AGENT
            :FORMAL ?!obj
            :AGENT ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODA1_1 rule with pronouns as both AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_1-robustPro-AGENT-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_1-robustPro-AGENT-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODA1_1 rule with no AGENT and a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_1-robustPro-FORMAL-only>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_1-robustPro-FORMAL-only
            :AGENT -
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODA1_1 rule with a pronoun as AGENT and no FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_1-robustPro-AGENT-only>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_1-robustPro-AGENT-only
            :AGENT ?!ag
            :FORMAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule30_4_AGENT_FORMAL-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule30_4_AGENT_FORMAL-MODA1_2 rule with AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_2>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_2
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule30_4_AGENT_FORMAL-MODA1_2 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_2-AGENT>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_2-AGENT
            :AGENT ?!ag
            :FORMAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODA1_2 rule with only FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_2-FORMAL>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_2-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule30_4_AGENT_FORMAL-MODA1_2 rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_2-robust-AGENT>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_2-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule30_4_AGENT_FORMAL-MODA1_2-robust-AGENT-TERM
            )
           )
          ;; rule30_4_AGENT_FORMAL-MODA1_2 rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_2-robust-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_2-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule30_4_AGENT_FORMAL-MODA1_2-robust-FORMAL-TERM
            ))
          ;; rule30_4_AGENT_FORMAL-MODA1_2 rule with neither AGENT nor FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_2-none>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_2-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODA1_2 rule with known AGENT but a reflexive pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_2-robustProSELF-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_2-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODA1_2 rule with known AGENT but a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_2-robustPro-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_2-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODA1_2 rule with a pronoun as AGENT but a known FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_2-robustPro-AGENT>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_2-robustPro-AGENT
            :FORMAL ?!obj
            :AGENT ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODA1_2 rule with pronouns as both AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_2-robustPro-AGENT-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_2-robustPro-AGENT-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODA1_2 rule with no AGENT and a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_2-robustPro-FORMAL-only>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_2-robustPro-FORMAL-only
            :AGENT -
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODA1_2 rule with a pronoun as AGENT and no FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODA1_2-robustPro-AGENT-only>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODA1_2-robustPro-AGENT-only
            :AGENT ?!ag
            :FORMAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule30_4_AGENT_FORMAL-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule30_4_AGENT_FORMAL-MODN1_1 rule with AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_1>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_1
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule30_4_AGENT_FORMAL-MODN1_1 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_1-AGENT>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_1-AGENT
            :AGENT ?!ag
            :FORMAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODN1_1 rule with only FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_1-FORMAL>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_1-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule30_4_AGENT_FORMAL-MODN1_1 rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_1-robust-AGENT>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_1-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule30_4_AGENT_FORMAL-MODN1_1-robust-AGENT-TERM
            )
           )
          ;; rule30_4_AGENT_FORMAL-MODN1_1 rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_1-robust-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_1-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule30_4_AGENT_FORMAL-MODN1_1-robust-FORMAL-TERM
            ))
          ;; rule30_4_AGENT_FORMAL-MODN1_1 rule with neither AGENT nor FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_1-none>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_1-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODN1_1 rule with known AGENT but a reflexive pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_1-robustProSELF-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_1-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODN1_1 rule with known AGENT but a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_1-robustPro-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_1-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODN1_1 rule with a pronoun as AGENT but a known FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_1-robustPro-AGENT>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_1-robustPro-AGENT
            :FORMAL ?!obj
            :AGENT ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODN1_1 rule with pronouns as both AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_1-robustPro-AGENT-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_1-robustPro-AGENT-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODN1_1 rule with no AGENT and a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_1-robustPro-FORMAL-only>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_1-robustPro-FORMAL-only
            :AGENT -
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODN1_1 rule with a pronoun as AGENT and no FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_1-robustPro-AGENT-only>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_1-robustPro-AGENT-only
            :AGENT ?!ag
            :FORMAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule30_4_AGENT_FORMAL-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule30_4_AGENT_FORMAL-MODN1_2 rule with AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_2>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_2
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule30_4_AGENT_FORMAL-MODN1_2 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_2-AGENT>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_2-AGENT
            :AGENT ?!ag
            :FORMAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODN1_2 rule with only FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_2-FORMAL>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_2-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule30_4_AGENT_FORMAL-MODN1_2 rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_2-robust-AGENT>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_2-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule30_4_AGENT_FORMAL-MODN1_2-robust-AGENT-TERM
            )
           )
          ;; rule30_4_AGENT_FORMAL-MODN1_2 rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_2-robust-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_2-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule30_4_AGENT_FORMAL-MODN1_2-robust-FORMAL-TERM
            ))
          ;; rule30_4_AGENT_FORMAL-MODN1_2 rule with neither AGENT nor FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_2-none>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_2-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODN1_2 rule with known AGENT but a reflexive pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_2-robustProSELF-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_2-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODN1_2 rule with known AGENT but a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_2-robustPro-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_2-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODN1_2 rule with a pronoun as AGENT but a known FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_2-robustPro-AGENT>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_2-robustPro-AGENT
            :FORMAL ?!obj
            :AGENT ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODN1_2 rule with pronouns as both AGENT and FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_2-robustPro-AGENT-FORMAL>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_2-robustPro-AGENT-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODN1_2 rule with no AGENT and a pronoun as FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_2-robustPro-FORMAL-only>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_2-robustPro-FORMAL-only
            :AGENT -
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule30_4_AGENT_FORMAL-MODN1_2 rule with a pronoun as AGENT and no FORMAL
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_4_AGENT_FORMAL))
           -rule30_4_AGENT_FORMAL-MODN1_2-robustPro-AGENT-only>
           29
           (ONT::event ?ev ?!eventName
            :rule -rule30_4_AGENT_FORMAL-MODN1_2-robustPro-AGENT-only
            :AGENT ?!ag
            :FORMAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule25_4_AGENT_AFFECTED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule25_4_AGENT_AFFECTED rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule25_4_AGENT_AFFECTED rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-AGENT>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-AFFECTED>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule25_4_AGENT_AFFECTED rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-robust-AGENT>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule25_4_AGENT_AFFECTED-robust-AGENT-TERM
            )
           )
          ;; rule25_4_AGENT_AFFECTED rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-robust-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule25_4_AGENT_AFFECTED-robust-AFFECTED-TERM
            ))
          ;; rule25_4_AGENT_AFFECTED rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-none>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-robustProSELF-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-robustPro-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-robustPro-AGENT>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-robustPro-AGENT-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-robustPro-AFFECTED-only>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-robustPro-AGENT-only>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule25_4_AGENT_AFFECTED-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule25_4_AGENT_AFFECTED-POLARITY1-gd rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-POLARITY1-gd>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-POLARITY1-gd
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule25_4_AGENT_AFFECTED-POLARITY1-gd rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-POLARITY1-gd-AGENT>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-POLARITY1-gd-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-POLARITY1-gd rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-POLARITY1-gd-AFFECTED>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-POLARITY1-gd-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule25_4_AGENT_AFFECTED-POLARITY1-gd rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robust-AGENT>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robust-AGENT-TERM
            )
           )
          ;; rule25_4_AGENT_AFFECTED-POLARITY1-gd rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robust-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robust-AFFECTED-TERM
            ))
          ;; rule25_4_AGENT_AFFECTED-POLARITY1-gd rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-POLARITY1-gd-none>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-POLARITY1-gd-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-POLARITY1-gd rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robustProSELF-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-POLARITY1-gd rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-POLARITY1-gd rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-POLARITY1-gd rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-POLARITY1-gd rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AFFECTED-only>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-POLARITY1-gd rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT-only>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule25_4_AGENT_AFFECTED-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule25_4_AGENT_AFFECTED-MODA1_1 rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_1>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_1
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule25_4_AGENT_AFFECTED-MODA1_1 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_1-AGENT>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_1-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODA1_1 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_1-AFFECTED>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_1-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule25_4_AGENT_AFFECTED-MODA1_1 rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_1-robust-AGENT>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_1-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule25_4_AGENT_AFFECTED-MODA1_1-robust-AGENT-TERM
            )
           )
          ;; rule25_4_AGENT_AFFECTED-MODA1_1 rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_1-robust-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_1-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule25_4_AGENT_AFFECTED-MODA1_1-robust-AFFECTED-TERM
            ))
          ;; rule25_4_AGENT_AFFECTED-MODA1_1 rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_1-none>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_1-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODA1_1 rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_1-robustProSELF-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_1-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODA1_1 rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_1-robustPro-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_1-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODA1_1 rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODA1_1 rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODA1_1 rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_1-robustPro-AFFECTED-only>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_1-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODA1_1 rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT-only>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule25_4_AGENT_AFFECTED-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule25_4_AGENT_AFFECTED-MODA1_2 rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_2>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_2
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule25_4_AGENT_AFFECTED-MODA1_2 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_2-AGENT>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_2-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODA1_2 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_2-AFFECTED>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_2-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule25_4_AGENT_AFFECTED-MODA1_2 rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_2-robust-AGENT>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_2-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule25_4_AGENT_AFFECTED-MODA1_2-robust-AGENT-TERM
            )
           )
          ;; rule25_4_AGENT_AFFECTED-MODA1_2 rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_2-robust-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_2-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule25_4_AGENT_AFFECTED-MODA1_2-robust-AFFECTED-TERM
            ))
          ;; rule25_4_AGENT_AFFECTED-MODA1_2 rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_2-none>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_2-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODA1_2 rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_2-robustProSELF-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_2-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODA1_2 rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_2-robustPro-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_2-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODA1_2 rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODA1_2 rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODA1_2 rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_2-robustPro-AFFECTED-only>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_2-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODA1_2 rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT-only>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule25_4_AGENT_AFFECTED-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule25_4_AGENT_AFFECTED-MODN1_1 rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_1>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_1
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule25_4_AGENT_AFFECTED-MODN1_1 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_1-AGENT>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_1-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODN1_1 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_1-AFFECTED>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_1-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule25_4_AGENT_AFFECTED-MODN1_1 rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_1-robust-AGENT>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_1-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule25_4_AGENT_AFFECTED-MODN1_1-robust-AGENT-TERM
            )
           )
          ;; rule25_4_AGENT_AFFECTED-MODN1_1 rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_1-robust-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_1-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule25_4_AGENT_AFFECTED-MODN1_1-robust-AFFECTED-TERM
            ))
          ;; rule25_4_AGENT_AFFECTED-MODN1_1 rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_1-none>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_1-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODN1_1 rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_1-robustProSELF-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_1-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODN1_1 rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_1-robustPro-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_1-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODN1_1 rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODN1_1 rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODN1_1 rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_1-robustPro-AFFECTED-only>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_1-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODN1_1 rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT-only>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule25_4_AGENT_AFFECTED-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule25_4_AGENT_AFFECTED-MODN1_2 rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_2>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_2
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule25_4_AGENT_AFFECTED-MODN1_2 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_2-AGENT>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_2-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODN1_2 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_2-AFFECTED>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_2-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule25_4_AGENT_AFFECTED-MODN1_2 rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_2-robust-AGENT>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_2-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule25_4_AGENT_AFFECTED-MODN1_2-robust-AGENT-TERM
            )
           )
          ;; rule25_4_AGENT_AFFECTED-MODN1_2 rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_2-robust-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_2-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule25_4_AGENT_AFFECTED-MODN1_2-robust-AFFECTED-TERM
            ))
          ;; rule25_4_AGENT_AFFECTED-MODN1_2 rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_2-none>
           25
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_2-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODN1_2 rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_2-robustProSELF-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_2-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODN1_2 rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_2-robustPro-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_2-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODN1_2 rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODN1_2 rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT-AFFECTED>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODN1_2 rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_2-robustPro-AFFECTED-only>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_2-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule25_4_AGENT_AFFECTED-MODN1_2 rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CAUSE-EFFECT ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule25_4_AGENT_AFFECTED))
           -rule25_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT-only>
           24
           (ONT::event ?ev ?!eventName
            :rule -rule25_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule20_4_AGENT_AFFECTED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule20_4_AGENT_AFFECTED rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule20_4_AGENT_AFFECTED rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-AGENT>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-AFFECTED>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule20_4_AGENT_AFFECTED rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-robust-AGENT>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule20_4_AGENT_AFFECTED-robust-AGENT-TERM
            )
           )
          ;; rule20_4_AGENT_AFFECTED rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-robust-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule20_4_AGENT_AFFECTED-robust-AFFECTED-TERM
            ))
          ;; rule20_4_AGENT_AFFECTED rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-none>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-robustProSELF-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-robustPro-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-robustPro-AGENT>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-robustPro-AGENT-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-robustPro-AFFECTED-only>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-robustPro-AGENT-only>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule20_4_AGENT_AFFECTED-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule20_4_AGENT_AFFECTED-POLARITY1-gd rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-POLARITY1-gd>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-POLARITY1-gd
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule20_4_AGENT_AFFECTED-POLARITY1-gd rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-POLARITY1-gd-AGENT>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-POLARITY1-gd-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-POLARITY1-gd rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-POLARITY1-gd-AFFECTED>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-POLARITY1-gd-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule20_4_AGENT_AFFECTED-POLARITY1-gd rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robust-AGENT>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robust-AGENT-TERM
            )
           )
          ;; rule20_4_AGENT_AFFECTED-POLARITY1-gd rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robust-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robust-AFFECTED-TERM
            ))
          ;; rule20_4_AGENT_AFFECTED-POLARITY1-gd rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-POLARITY1-gd-none>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-POLARITY1-gd-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-POLARITY1-gd rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robustProSELF-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-POLARITY1-gd rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-POLARITY1-gd rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-POLARITY1-gd rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-POLARITY1-gd rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AFFECTED-only>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-POLARITY1-gd rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT-only>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule20_4_AGENT_AFFECTED-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule20_4_AGENT_AFFECTED-MODA1_1 rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_1>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_1
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule20_4_AGENT_AFFECTED-MODA1_1 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_1-AGENT>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_1-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODA1_1 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_1-AFFECTED>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_1-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule20_4_AGENT_AFFECTED-MODA1_1 rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_1-robust-AGENT>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_1-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule20_4_AGENT_AFFECTED-MODA1_1-robust-AGENT-TERM
            )
           )
          ;; rule20_4_AGENT_AFFECTED-MODA1_1 rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_1-robust-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_1-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule20_4_AGENT_AFFECTED-MODA1_1-robust-AFFECTED-TERM
            ))
          ;; rule20_4_AGENT_AFFECTED-MODA1_1 rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_1-none>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_1-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODA1_1 rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_1-robustProSELF-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_1-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODA1_1 rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_1-robustPro-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_1-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODA1_1 rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODA1_1 rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODA1_1 rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_1-robustPro-AFFECTED-only>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_1-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODA1_1 rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT-only>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule20_4_AGENT_AFFECTED-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule20_4_AGENT_AFFECTED-MODA1_2 rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_2>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_2
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule20_4_AGENT_AFFECTED-MODA1_2 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_2-AGENT>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_2-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODA1_2 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_2-AFFECTED>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_2-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule20_4_AGENT_AFFECTED-MODA1_2 rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_2-robust-AGENT>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_2-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule20_4_AGENT_AFFECTED-MODA1_2-robust-AGENT-TERM
            )
           )
          ;; rule20_4_AGENT_AFFECTED-MODA1_2 rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_2-robust-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_2-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule20_4_AGENT_AFFECTED-MODA1_2-robust-AFFECTED-TERM
            ))
          ;; rule20_4_AGENT_AFFECTED-MODA1_2 rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_2-none>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_2-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODA1_2 rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_2-robustProSELF-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_2-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODA1_2 rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_2-robustPro-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_2-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODA1_2 rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODA1_2 rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODA1_2 rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_2-robustPro-AFFECTED-only>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_2-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODA1_2 rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT-only>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule20_4_AGENT_AFFECTED-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule20_4_AGENT_AFFECTED-MODN1_1 rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_1>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_1
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule20_4_AGENT_AFFECTED-MODN1_1 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_1-AGENT>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_1-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODN1_1 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_1-AFFECTED>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_1-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule20_4_AGENT_AFFECTED-MODN1_1 rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_1-robust-AGENT>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_1-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule20_4_AGENT_AFFECTED-MODN1_1-robust-AGENT-TERM
            )
           )
          ;; rule20_4_AGENT_AFFECTED-MODN1_1 rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_1-robust-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_1-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule20_4_AGENT_AFFECTED-MODN1_1-robust-AFFECTED-TERM
            ))
          ;; rule20_4_AGENT_AFFECTED-MODN1_1 rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_1-none>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_1-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODN1_1 rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_1-robustProSELF-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_1-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODN1_1 rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_1-robustPro-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_1-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODN1_1 rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODN1_1 rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODN1_1 rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_1-robustPro-AFFECTED-only>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_1-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODN1_1 rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT-only>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule20_4_AGENT_AFFECTED-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule20_4_AGENT_AFFECTED-MODN1_2 rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_2>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_2
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule20_4_AGENT_AFFECTED-MODN1_2 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_2-AGENT>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_2-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODN1_2 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_2-AFFECTED>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_2-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule20_4_AGENT_AFFECTED-MODN1_2 rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_2-robust-AGENT>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_2-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule20_4_AGENT_AFFECTED-MODN1_2-robust-AGENT-TERM
            )
           )
          ;; rule20_4_AGENT_AFFECTED-MODN1_2 rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_2-robust-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_2-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule20_4_AGENT_AFFECTED-MODN1_2-robust-AFFECTED-TERM
            ))
          ;; rule20_4_AGENT_AFFECTED-MODN1_2 rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_2-none>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_2-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODN1_2 rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_2-robustProSELF-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_2-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODN1_2 rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_2-robustPro-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_2-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODN1_2 rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODN1_2 rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT-AFFECTED>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODN1_2 rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_2-robustPro-AFFECTED-only>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_2-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule20_4_AGENT_AFFECTED-MODN1_2 rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::MOTION ONT::ENTERING ONT::PUT ONT::PUSH ONT::PULL ONT::RELEASING ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_4_AGENT_AFFECTED))
           -rule20_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT-only>
           19
           (ONT::event ?ev ?!eventName
            :rule -rule20_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule10_4_AGENT_AFFECTED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule10_4_AGENT_AFFECTED rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AGENT_AFFECTED rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-AGENT>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-AFFECTED>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AGENT_AFFECTED rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-robust-AGENT>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule10_4_AGENT_AFFECTED-robust-AGENT-TERM
            )
           )
          ;; rule10_4_AGENT_AFFECTED rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-robust-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule10_4_AGENT_AFFECTED-robust-AFFECTED-TERM
            ))
          ;; rule10_4_AGENT_AFFECTED rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-none>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-robustProSELF-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-robustPro-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-robustPro-AGENT>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-robustPro-AGENT-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-robustPro-AFFECTED-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-robustPro-AGENT-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule10_4_AGENT_AFFECTED-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule10_4_AGENT_AFFECTED-POLARITY1-gd rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-POLARITY1-gd>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-POLARITY1-gd
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AGENT_AFFECTED-POLARITY1-gd rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-POLARITY1-gd-AGENT>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-POLARITY1-gd-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-POLARITY1-gd rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-POLARITY1-gd-AFFECTED>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-POLARITY1-gd-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AGENT_AFFECTED-POLARITY1-gd rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robust-AGENT>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robust-AGENT-TERM
            )
           )
          ;; rule10_4_AGENT_AFFECTED-POLARITY1-gd rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robust-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robust-AFFECTED-TERM
            ))
          ;; rule10_4_AGENT_AFFECTED-POLARITY1-gd rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-POLARITY1-gd-none>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-POLARITY1-gd-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-POLARITY1-gd rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robustProSELF-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-POLARITY1-gd rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-POLARITY1-gd rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-POLARITY1-gd rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-POLARITY1-gd rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AFFECTED-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-POLARITY1-gd rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-POLARITY1-gd-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule10_4_AGENT_AFFECTED-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule10_4_AGENT_AFFECTED-MODA1_1 rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_1>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_1
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AGENT_AFFECTED-MODA1_1 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_1-AGENT>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_1-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODA1_1 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_1-AFFECTED>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_1-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AGENT_AFFECTED-MODA1_1 rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_1-robust-AGENT>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_1-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule10_4_AGENT_AFFECTED-MODA1_1-robust-AGENT-TERM
            )
           )
          ;; rule10_4_AGENT_AFFECTED-MODA1_1 rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_1-robust-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_1-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule10_4_AGENT_AFFECTED-MODA1_1-robust-AFFECTED-TERM
            ))
          ;; rule10_4_AGENT_AFFECTED-MODA1_1 rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_1-none>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_1-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODA1_1 rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_1-robustProSELF-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_1-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODA1_1 rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_1-robustPro-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_1-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODA1_1 rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODA1_1 rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODA1_1 rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_1-robustPro-AFFECTED-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_1-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODA1_1 rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_1-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule10_4_AGENT_AFFECTED-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule10_4_AGENT_AFFECTED-MODA1_2 rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_2>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_2
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AGENT_AFFECTED-MODA1_2 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_2-AGENT>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_2-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODA1_2 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_2-AFFECTED>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_2-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AGENT_AFFECTED-MODA1_2 rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_2-robust-AGENT>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_2-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule10_4_AGENT_AFFECTED-MODA1_2-robust-AGENT-TERM
            )
           )
          ;; rule10_4_AGENT_AFFECTED-MODA1_2 rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_2-robust-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_2-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule10_4_AGENT_AFFECTED-MODA1_2-robust-AFFECTED-TERM
            ))
          ;; rule10_4_AGENT_AFFECTED-MODA1_2 rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_2-none>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_2-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODA1_2 rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_2-robustProSELF-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_2-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODA1_2 rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_2-robustPro-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_2-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODA1_2 rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODA1_2 rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODA1_2 rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_2-robustPro-AFFECTED-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_2-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODA1_2 rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODA1_2-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule10_4_AGENT_AFFECTED-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule10_4_AGENT_AFFECTED-MODN1_1 rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_1>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_1
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AGENT_AFFECTED-MODN1_1 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_1-AGENT>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_1-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODN1_1 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_1-AFFECTED>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_1-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AGENT_AFFECTED-MODN1_1 rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_1-robust-AGENT>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_1-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule10_4_AGENT_AFFECTED-MODN1_1-robust-AGENT-TERM
            )
           )
          ;; rule10_4_AGENT_AFFECTED-MODN1_1 rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_1-robust-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_1-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule10_4_AGENT_AFFECTED-MODN1_1-robust-AFFECTED-TERM
            ))
          ;; rule10_4_AGENT_AFFECTED-MODN1_1 rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_1-none>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_1-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODN1_1 rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_1-robustProSELF-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_1-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODN1_1 rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_1-robustPro-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_1-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODN1_1 rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODN1_1 rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODN1_1 rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_1-robustPro-AFFECTED-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_1-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODN1_1 rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_1-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule10_4_AGENT_AFFECTED-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule10_4_AGENT_AFFECTED-MODN1_2 rule with AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_2>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_2
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AGENT_AFFECTED-MODN1_2 rule with only AGENT
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_2-AGENT>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_2-AGENT
            :AGENT ?!ag
            :AFFECTED -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODN1_2 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_2-AFFECTED>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_2-AFFECTED
            :AGENT -
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AGENT_AFFECTED-MODN1_2 rule with known AFFECTED but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_2-robust-AGENT>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_2-robust-AGENT
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule10_4_AGENT_AFFECTED-MODN1_2-robust-AGENT-TERM
            )
           )
          ;; rule10_4_AGENT_AFFECTED-MODN1_2 rule with known AGENT but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_2-robust-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_2-robust-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule10_4_AGENT_AFFECTED-MODN1_2-robust-AFFECTED-TERM
            ))
          ;; rule10_4_AGENT_AFFECTED-MODN1_2 rule with neither AGENT nor AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT - :AFFECTED - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_2-none>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_2-none
;            :AGENT ?!ag
;            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODN1_2 rule with known AGENT but a reflexive pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_2-robustProSELF-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_2-robustProSELF-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODN1_2 rule with known AGENT but a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_2-robustPro-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_2-robustPro-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODN1_2 rule with a pronoun as AGENT but a known AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT
            :AFFECTED ?!obj
            :AGENT ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODN1_2 rule with pronouns as both AGENT and AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT-AFFECTED
            :AGENT ?!ag
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODN1_2 rule with no AGENT and a pronoun as AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_2-robustPro-AFFECTED-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_2-robustPro-AFFECTED-only
            :AGENT -
            :AFFECTED ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AGENT_AFFECTED-MODN1_2 rule with a pronoun as AGENT and no AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::MAINTAIN-KEEP ONT::ACTIVITY-ONGOING ONT::RETAIN ONT::STAY ) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AGENT_AFFECTED))
           -rule10_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AGENT_AFFECTED-MODN1_2-robustPro-AGENT-only
            :AGENT ?!ag
            :AFFECTED -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule10_4_AFFECTED_AFFECTED1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule10_4_AFFECTED_AFFECTED1 rule with AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-AFFECTED>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1 rule with only AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-AFFECTED1>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-AFFECTED1
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1 rule with known AFFECTED1 but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-robust-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-robust-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule10_4_AFFECTED_AFFECTED1-robust-AFFECTED-TERM
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1 rule with known AFFECTED but unknown proper name argument as AFFECTED1
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-robust-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-robust-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule10_4_AFFECTED_AFFECTED1-robust-AFFECTED1-TERM
            ))
          ;; rule10_4_AFFECTED_AFFECTED1 rule with neither AFFECTED nor AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED - :AFFECTED1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-none>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-none
;            :AFFECTED ?!ag
;            :AFFECTED1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1 rule with known AFFECTED but a reflexive pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-robustProSELF-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-robustProSELF-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1 rule with known AFFECTED but a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-robustPro-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-robustPro-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1 rule with a pronoun as AFFECTED but a known AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-robustPro-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-robustPro-AFFECTED
            :AFFECTED1 ?!obj
            :AFFECTED ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1 rule with pronouns as both AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-robustPro-AFFECTED-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-robustPro-AFFECTED-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1 rule with no AFFECTED and a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-robustPro-AFFECTED1-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-robustPro-AFFECTED1-only
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1 rule with a pronoun as AFFECTED and no AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-robustPro-AFFECTED-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-robustPro-AFFECTED-only
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-AFFECTED>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with only AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-AFFECTED1>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-AFFECTED1
            :AFFECTED -
            :AFFECTED1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with known AFFECTED1 but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robust-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robust-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robust-AFFECTED-TERM
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with known AFFECTED but unknown proper name argument as AFFECTED1
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robust-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robust-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robust-AFFECTED1-TERM
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with neither AFFECTED nor AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED - :AFFECTED1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-none>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-none
;            :AFFECTED ?!ag
;            :AFFECTED1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with known AFFECTED but a reflexive pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustProSELF-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustProSELF-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with known AFFECTED but a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with a pronoun as AFFECTED but a known AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED
            :AFFECTED1 ?!obj
            :AFFECTED ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with pronouns as both AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with no AFFECTED and a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED1-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED1-only
            :AFFECTED -
            :AFFECTED1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd rule with a pronoun as AFFECTED and no AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-POLARITY1-gd-robustPro-AFFECTED-only
            :AFFECTED ?!ag
            :AFFECTED1 -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule10_4_AFFECTED_AFFECTED1-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_1 rule with AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_1>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_1 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_1-AFFECTED>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_1-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_1 rule with only AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_1-AFFECTED1>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_1-AFFECTED1
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_1 rule with known AFFECTED1 but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robust-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robust-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robust-AFFECTED-TERM
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_1 rule with known AFFECTED but unknown proper name argument as AFFECTED1
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robust-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robust-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robust-AFFECTED1-TERM
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_1 rule with neither AFFECTED nor AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED - :AFFECTED1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_1-none>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_1-none
;            :AFFECTED ?!ag
;            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_1 rule with known AFFECTED but a reflexive pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robustProSELF-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robustProSELF-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_1 rule with known AFFECTED but a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_1 rule with a pronoun as AFFECTED but a known AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED
            :AFFECTED1 ?!obj
            :AFFECTED ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_1 rule with pronouns as both AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_1 rule with no AFFECTED and a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED1-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED1-only
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_1 rule with a pronoun as AFFECTED and no AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modA))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_1-robustPro-AFFECTED-only
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule10_4_AFFECTED_AFFECTED1-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_2 rule with AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_2>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_2
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_2 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_2-AFFECTED>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_2-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_2 rule with only AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_2-AFFECTED1>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_2-AFFECTED1
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_2 rule with known AFFECTED1 but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robust-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robust-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robust-AFFECTED-TERM
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_2 rule with known AFFECTED but unknown proper name argument as AFFECTED1
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robust-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robust-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robust-AFFECTED1-TERM
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_2 rule with neither AFFECTED nor AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED - :AFFECTED1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_2-none>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_2-none
;            :AFFECTED ?!ag
;            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_2 rule with known AFFECTED but a reflexive pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robustProSELF-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robustProSELF-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_2 rule with known AFFECTED but a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_2 rule with a pronoun as AFFECTED but a known AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED
            :AFFECTED1 ?!obj
            :AFFECTED ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_2 rule with pronouns as both AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_2 rule with no AFFECTED and a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED1-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED1-only
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODA1_2 rule with a pronoun as AFFECTED and no AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modA)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODA1_2-robustPro-AFFECTED-only
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule10_4_AFFECTED_AFFECTED1-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_1 rule with AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_1>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_1 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_1-AFFECTED>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_1-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_1 rule with only AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_1-AFFECTED1>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_1-AFFECTED1
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_1 rule with known AFFECTED1 but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robust-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robust-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robust-AFFECTED-TERM
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_1 rule with known AFFECTED but unknown proper name argument as AFFECTED1
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robust-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robust-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robust-AFFECTED1-TERM
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_1 rule with neither AFFECTED nor AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED - :AFFECTED1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_1-none>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_1-none
;            :AFFECTED ?!ag
;            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_1 rule with known AFFECTED but a reflexive pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robustProSELF-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robustProSELF-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_1 rule with known AFFECTED but a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_1 rule with a pronoun as AFFECTED but a known AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED
            :AFFECTED1 ?!obj
            :AFFECTED ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_1 rule with pronouns as both AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_1 rule with no AFFECTED and a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED1-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED1-only
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_1 rule with a pronoun as AFFECTED and no AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MODS (?!modN))
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_1-robustPro-AFFECTED-only
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule10_4_AFFECTED_AFFECTED1-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_2 rule with AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_2>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_2
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_2 rule with only AFFECTED
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_2-AFFECTED>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_2-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_2 rule with only AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_2-AFFECTED1>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_2-AFFECTED1
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_2 rule with known AFFECTED1 but unknown proper name argument as AFFECTED
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robust-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robust-AFFECTED
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robust-AFFECTED-TERM
            )
           )
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_2 rule with known AFFECTED but unknown proper name argument as AFFECTED1
          ;; (includes a TERM extraction for the unknown argument)
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robust-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robust-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robust-AFFECTED1-TERM
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_2 rule with neither AFFECTED nor AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED - :AFFECTED1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
;           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
;           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_2-none>
           10
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_2-none
;            :AFFECTED ?!ag
;            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_2 rule with known AFFECTED but a reflexive pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robustProSELF-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robustProSELF-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_2 rule with known AFFECTED but a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_2 rule with a pronoun as AFFECTED but a known AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (?reln2 ?!obj (? t2 ONT::REFERENTIAL-SEM))
           (ONT::TERM ?!ag ?!agtype :PRO (? pro W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED
            :AFFECTED1 ?!obj
            :AFFECTED ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_2 rule with pronouns as both AFFECTED and AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO (? pro1 W::I W::you W::we W::it W::they W::he W::she W::*you* W::this W::that w::what w::which))
           (ONT::TERM ?!obj ?!objtype :PRO (? pro2 W::me W::you W::us W::it W::them W::him W::her W::*you* W::this W::that w::what w::which))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED-AFFECTED1>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED-AFFECTED1
            :AFFECTED ?!ag
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_2 rule with no AFFECTED and a pronoun as AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!obj ?!objtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED1-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED1-only
            :AFFECTED -
            :AFFECTED1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule10_4_AFFECTED_AFFECTED1-MODN1_2 rule with a pronoun as AFFECTED and no AFFECTED1
          ((?!reln0 ?ev
            (? type ONT::CHANGE ONT::BECOME ) :AFFECTED ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - :MANNER ?!modN)
           (ONT::TERM ?!ag ?!agtype :PRO ?!pro)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule10_4_AFFECTED_AFFECTED1))
           -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED-only>
           9
           (ONT::event ?ev ?!eventName
            :rule -rule10_4_AFFECTED_AFFECTED1-MODN1_2-robustPro-AFFECTED-only
            :AFFECTED ?!ag
            :AFFECTED1 -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?var_degree
            :FREQUENCY ?var_freq
            :TYPE ?type
            :DRUM ?code
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; END: rules automatically generated from template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	  )
	)


