
(in-package :im)

;;  e.g. (setq *symbol-map* '((ONT::DOG CANINE) (ONT::CAT FELINE)))
;;      would bind the variable ?y to IM::CANINE in a extraction rule term of form  (ONT::EVAL (symbolmap ONT::DOG ?y))

;(setq *symbol-map* nil)

(setq *symbol-map* 
      '(
	(ONT::POLARITY-VAL-POSITIVE ONT::POSITIVE)
	(ONT::POLARITY-VAL-NEGATIVE ONT::NEGATIVE)
	(ONT::NECESSARY ONT::DEPENDENT)
	(ONT::ADEQUATE ONT::ENSURE)
    (ONT::QUANTITY-ABSTR ONT::QTY) ; only in cwms
        (ONT::CREATE ONT::CREATE -rule40_4_AGENT_AFFECTED)
         (ONT::CAUSE-PRODUCE-REPRODUCE ONT::CREATE -rule40_4_AGENT_AFFECTED)
         (ONT::BE-BORN ONT::CREATE -rule40_4_AGENT_AFFECTED)
         (ONT::APPEAR ONT::CREATE -rule40_4_AGENT_AFFECTED)
        (ONT::STOP ONT::DESTROY -rule40_4_AGENT_AFFECTED)
         (ONT::CONSUME ONT::DESTROY -rule40_4_AGENT_AFFECTED)
         (ONT::TAKE-IN ONT::DESTROY -rule40_4_AGENT_AFFECTED)
         (ONT::BREAK-OBJECT ONT::DESTROY -rule40_4_AGENT_AFFECTED)
         (ONT::RENDER-INEFFECTIVE ONT::DESTROY -rule40_4_AGENT_AFFECTED)
         (ONT::DECREASE-COMPLETELY ONT::DESTROY -rule40_4_AGENT_AFFECTED)
         (ONT::DISAPPEAR ONT::DESTROY -rule40_4_AGENT_AFFECTED)
         (ONT::CHANGE-INTEGRITY ONT::DESTROY -rule40_4_AGENT_AFFECTED)
         (ONT::DIE ONT::DESTROY -rule40_4_AGENT_AFFECTED)
         (ONT::DAMAGE ONT::DESTROY -rule40_4_AGENT_AFFECTED)
         (ONT::DESTROY ONT::DESTROY -rule40_4_AGENT_AFFECTED)
         (ONT::EXPLODE ONT::DESTROY -rule40_4_AGENT_AFFECTED)
         (ONT::DISCARD ONT::DESTROY -rule40_4_AGENT_AFFECTED)
        (ONT::CREATE ONT::CREATE -rule40_4_AGENT_AFFECTED-RESULT)
         (ONT::CAUSE-PRODUCE-REPRODUCE ONT::CREATE -rule40_4_AGENT_AFFECTED-RESULT)
         (ONT::BE-BORN ONT::CREATE -rule40_4_AGENT_AFFECTED-RESULT)
        (ONT::BREAK-OBJECT ONT::DESTROY -rule40_4_AFFECTED_AFFECTED1)
        (ONT::RENDER-INEFFECTIVE ONT::DESTROY -rule40_4_AGENT_FORMAL)
        (ONT::CAUSE-EFFECT ONT::CREATE -rule30_4_AGENT_FORMAL)
        (ONT::CAUSE-EFFECT ONT::CREATE -rule25_4_AGENT_AFFECTED)
        (ONT::MOTION ONT::MOVE -rule20_4_AGENT_AFFECTED)
         (ONT::PUT ONT::MOVE -rule20_4_AGENT_AFFECTED)
         (ONT::PUSH ONT::MOVE -rule20_4_AGENT_AFFECTED)
         (ONT::PULL ONT::MOVE -rule20_4_AGENT_AFFECTED)
         (ONT::RELEASING ONT::MOVE -rule20_4_AGENT_AFFECTED)
         (ONT::GIVING ONT::MOVE -rule20_4_AGENT_AFFECTED)
         (ONT::ENTERING ONT::MOVE -rule20_4_AGENT_AFFECTED)
         (ONT::CAUSE-IN ONT::MOVE -rule20_4_AGENT_AFFECTED)
         (ONT::CAUSE-COME-FROM ONT::MOVE -rule20_4_AGENT_AFFECTED)
         (ONT::FILL-CONTAINER ONT::MOVE -rule20_4_AGENT_AFFECTED)
         (ONT::GUIDING ONT::MOVE -rule20_4_AGENT_AFFECTED)
         (ONT::INHALE ONT::MOVE -rule20_4_AGENT_AFFECTED)
         (ONT::EXHALE ONT::MOVE -rule20_4_AGENT_AFFECTED)
         (ONT::EXCRETE ONT::MOVE -rule20_4_AGENT_AFFECTED)
         (ONT::ROTATE ONT::MOVE -rule20_4_AGENT_AFFECTED)
        (ONT::ENTERING ONT::MOVE -rule20_4_AGENT_NEUTRAL)
         (ONT::DEPART ONT::MOVE -rule20_4_AGENT_NEUTRAL)
         (ONT::REACH ONT::MOVE -rule20_4_AGENT_NEUTRAL)
        (ONT::GIVING ONT::MOVE -rule20_4_AGENT_AFFECTED-RESULT)
         (ONT::FILL-CONTAINER ONT::MOVE -rule20_4_AGENT_AFFECTED-RESULT)
        (ONT::INCUR-INHERIT-RECEIVE ONT::MOVE -rule20_4_AFFECTED_AFFECTED1)
         (ONT::INHALE ONT::MOVE -rule20_4_AFFECTED_AFFECTED1)
         (ONT::EXHALE ONT::MOVE -rule20_4_AFFECTED_AFFECTED1)
         (ONT::EXCRETE ONT::MOVE -rule20_4_AFFECTED_AFFECTED1)
        (ONT::CHANGE ONT::CHANGE -rule10_4_AGENT_AFFECTED)
        (ONT::MAINTAIN-KEEP ONT::NO-CHANGE -rule10_4_AGENT_AFFECTED)
         (ONT::ACTIVITY-ONGOING ONT::NO-CHANGE -rule10_4_AGENT_AFFECTED)
         (ONT::RETAIN ONT::NO-CHANGE -rule10_4_AGENT_AFFECTED)
         (ONT::STAY ONT::NO-CHANGE -rule10_4_AGENT_AFFECTED)
        (ONT::CHANGE ONT::CHANGE -rule10_4_AFFECTED_AFFECTED1)
         (ONT::BECOME ONT::CHANGE -rule10_4_AFFECTED_AFFECTED1)
	)
)
