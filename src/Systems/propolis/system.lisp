;;;; src/Systems/propolis/system.lisp
;;;; Defines and loads an instance of the TRIPS system.

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;Systems;core;system")

(trips:def-trips-system :propolis
  (:dfc-component	:lxm		#!TRIPS"src;LexiconManager;")
  (:dfc-component	:parser		#!TRIPS"src;Parser;")
  (:dfc-component	:im		#!TRIPS"src;NewIM;")
  (:dfc-component	:domiknows	#!TRIPS"src;Domiknows;")
  (:dfc-component	:webparser	#!TRIPS"src;WebParser;")
  )

;; Now load the system
(trips:load-trips-system)

(defun parse-eval (x)
  (im::send-msg `(request :receiver parser :content (eval ,x))))

;;;; extractor rules
(load #!TRIPS"src;Systems;propolis;DRUMRules_ev.lisp")
(load #!TRIPS"src;Systems;propolis;cwmsRules_ev_add.lisp")
(load #!TRIPS"src;Systems;propolis;DRUMRules_ev_add.lisp")
(load #!TRIPS"src;Systems;propolis;postprocessRules.lisp")
(load #!TRIPS"src;Systems;propolis;emptyRules.lisp")
(load #!TRIPS"src;Systems;propolis;symbolmapping.lisp")
(setq im::*roles-to-emit* nil)
