;;;; src/Systems/propolis/test.lisp

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))

;;;
;;; Load our TRIPS system
;;;
(load #!TRIPS"src;Systems;propolis;system")

;;;
;;; Load core testing code
;;;
(load #!TRIPS"src;Systems;core;test")

;;;
;;; In USER for convenience of testing
;;;
(in-package :common-lisp-user)

;;;
;;; Sample dialogues
;;;
(setf *sample-dialogues* nil) ; TBD

(setf *texttagger-split-mode* :split-sentences)

(defun ptest (key)
  "Make the sample dialogue given by KEY the global *TEST-DIALOG*, then
call TEST. Reports available KEYs on error."
  (let ((dialogue (cdr (assoc key *sample-dialogues* :test #'eql))))
    (cond
     ((not dialogue)
      (format t "~&ptest: unknown sample dialogue: ~S~%" key)
      (format t "~&ptest: possible values: ~S~%" (mapcar #'car *sample-dialogues*)))
     (t
      (setf *test-dialog* dialogue)
      (test)))))

(defun test-paragraph (p &key (evaluate-lfs t) (type 'default))
  "Send the given paragraph through TextTagger to be split into clauses and sent through the rest of the system.
   @param p Either a string containing the paragraph, or a symbol associated with the paragraph record in *sample-dialogues*
   @param :evaluate-lfs Set this to nil if you don't want to evaluate the LF for the whole paragraph against the gold LF
   @param :type The :type argument to TextTagger (see src/TextTagger/README)"
  (cond
   ((stringp p)
    (COMM::send 'test `(request :content (tag :text ,p :imitate-keyboard-manager t ,*texttagger-split-mode* t :type ,type))))
   ((symbolp p)
    (let* ((paragraph (find p *sample-dialogues* :key #'second))
           (text (comm::get-keyword-arg paragraph :text))
	   (gold-lf (comm::get-keyword-arg paragraph :terms))
	   (sense-bracketing
	     (remove nil
	             (mapcar (lambda (s)
		               (comm::get-keyword-arg s :sense-bracketing))
		             (mapcan (lambda (p)
			               (comm::get-keyword-arg p :terms))
			             gold-lf)
			     )))
	   )
      (when (and gold-lf evaluate-lfs)
        (COMM::send 'test `(tell :content (paragraph-gold-lfs :content ,gold-lf))))
      (COMM::send 'test `(request :content (tag :text ,text :imitate-keyboard-manager t ,*texttagger-split-mode* t :type ,type :sense-bracketing ,sense-bracketing)))
      ))
   ))

(defun enable-graphviz-display ()
  (COMM::send 'test '(request :receiver graphviz :content (enable-display))))
(defun disable-graphviz-display ()
  (COMM::send 'test '(request :receiver graphviz :content (disable-display))))

