;;;
;;; After-load customizations for this TRIPS system
;;;

(setq parser::*parser-init-settings*
      '((parser::*in-system* :propolis)
	(parser::*word-length* 7)      ;; guess and average number of letters in a word
;	(parser::*score-length-multiplier* 1)   ;;  boost factor for constituent size
	(parser::*score-length-multiplier* 0.4)   ;;  boost factor for constituent size
	(parser::*score-corner-multiplier* 0)    ;; not clear this is helpful
	(parser::*use-tags-as-filter* t)        ;;  indicate we should use POS information
	(parser::*bad-tag-multiplier* .98)       ;;  penalty multiplier for lex entries that do not match POS tags
	(parser::*skeleton-constit-cats* '(W::NP W::CP W::VP W::ADVBL W::S))  ;; constituents that we expect in the skeleton
	(parser::*skeleton-boost-factor* 0.1) ;0.2)   ;;  boost if we build a constituent that matches the skeleton (from stat. parser)
	;;(parser::*remove-subsumed-skeleton-constit* t)  ;; remove subsumed constits in skeleton (e.g., NP within NP) as they are unreliable predictors
	((setf (parser::barrier-penalty parser::*chart*) .995)) ;.99))        ;; this is the penalty for arcs that attempt to cross barriers from the preferences from stat parser
	(parser::*kr-type-info-desired* '(:WNsense))
	(parser::*add-lex-to-lf* t)
	(parser::*sem-features-to-output* '(F::container F::mobility))
	(parser::*add-top-level-type* t) ; add :top_type if type is under SITUATION-ROOT 
	(parser::*no-positions-in-lf* nil)       ;; generate start and end positions
	((parser::setmaxnumberentries 5000))    ;;  # constituents built before stopping
        ((parser::setmaxchartsize 5000))        ;;  max #  characters in any input
	((setf (parser::number-parses-to-find parser::*chart*) 20))
	((setf (parser::number-parses-desired parser::*chart*) 1)) ; DrumGUI can't handle new-speech-act-hyps, so must get only one hyp
	((setf (parser::flexible-semantic-matching parser::*chart*) t))  ;;  selection preferences rather than restrictions
	(parser::*include-parse-tree-in-messages* '(w::lex)) ;; required for WebParser
	(parser::*semantic-skeleton-scoring-enabled* nil) ; disable semantic scoring
	(parser::*rules-to-suppress* '(w::-vp-pastprt-adjp-attributive> w::-vp-pastprt-adjp->)) ; state-resulting-from
	
	((parser::customize-cost-table '((ont::SA_QUERY 2)
					 (ont::SA_IDENTIFY 1.2)
					 (ont::SA_pred-fragment 2) 
					 (ont::SA_request 1.1)
					 (ont::SA_YN-QUESTION 2)
					 (ont::SA_CONFIRM 1.3)
					 (ont::SA_WH-QUESTION 2)
					 (ont::SA_TELL 1)
					 (w::CP 1.5)
					 (w::VP 2) 
					 (w::punc .5)
					 (ONT::speech-act 1.2) ;; this occurs if we have an sequence of speech acts
					 )))
	))

(parser::initialize-settings)   ;; do it here to set the parser for testing (so it matches the settings of the parser component)

(setq im::*max-allowed-utts-in-turn* 20)  ;; basically try to do something with everything
(setq im::*external-name-resolution* nil) ;; no domain-specific reasoner
;;(setq im::*tma-suppress-list* '(w::passive)) ;; w::progr))  ;; suppress passive and progressive indicators
(setq im::*show-lf-graphs* t)    ;; turn on LF-graph displays for debugging
(setq im::*no-BA-mode* t)        ;; there is no behavioral agent in propolis

;; propolis uses text-tagger
(setq *use-texttagger* t)
; step settings
;(setq im::*current-dialog-manager* #'im::textIM)
;(setq im::*output-format* 'im::LF)
; cwmsreader settings (so DrumGUI will behave)
(setq im::*current-dialog-manager* #'im::extractsequenceIM)
(setq im::*output-format* 'im::lf-term)

;; we want to extract multiple events even when they share arguments
(setq im::*max-cover-with-strict-disjoint-extractions* nil)
;; we want to emit all events
(setq im::*eliminate-subevents* nil)

(setq im::*allow-optional-lfs* t) ;; set to t for optional term matching

;;;; extractor rules
(setq im::*extraction-sequence* '((im::preprocessRules) (im::drum) (im::sequence_add) (im::cwms_ev_add) (im::drum_ev_add) (im::postprocessRules) (im::postprocessRules2) (im::emptyrules)))
(setq im::*substitute-terms-in-extraction* t)
(setq im::*roles-to-emit* nil)
