;;;; Copyright (c) 2022 Anton Vodonosov (avodonosov@yandex.ru)
;;;; 
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;;; of this software and associated documentation files (the "Software"), to deal
;;;; in the Software without restriction, including without limitation the rights
;;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the Software is
;;;; furnished to do so, subject to the following conditions:
;;;; 
;;;; The above copyright notice and this permission notice shall be included in all
;;;; copies or substantial portions of the Software.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;;; SOFTWARE.

(defpackage #:package-doc-dump
  (:export #:dump-html)
  (:use cl))

;; Temporarily, until https://github.com/eudoxia0/docparser/pull/31
;; is released and
;; https://github.com/eudoxia0/docparser/issues/34
;; is fixed and released:
(in-package :docparser)

(defclass cffi-function (cffi-node function-node)
  ((foreign-name :reader cffi-function-foreign-name
                 :initarg :foreign-name
                 :type string
                 :documentation "The function's foreign name.")
   (return-type :reader cffi-function-return-type
                :initarg :return-type
                :documentation "The function's return type."))
  (:documentation "A C function."))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (export 'cffi-function-foreign-name))

(define-parser cffi:defcfun (name-and-options return-type &rest args)
  (multiple-value-bind (lisp-name foreign-name)
      (cffi::parse-name-and-options name-and-options)
    (let ((docstring (if (stringp (first args))
                         (first args)
                         nil))
          (args (if (stringp (first args))
                    (rest args)
                    args)))
      (make-instance 'cffi-function
                     :name lisp-name
                     :foreign-name foreign-name
                     :docstring docstring
                     :return-type return-type
                     :lambda-list args))))

(in-package #:package-doc-dump)

(defparameter *utf-8-external-format* asdf:*utf-8-external-format*
  "See docstring for ASDF:*UTF-8-EXTERNAL-FORMAT*.")

(defparameter *utf-8-compatible-character-type*
  #+lispworks 'lw:simple-char
  #-lispworks 'character)

(defun package-related-forms (input-stream)
  "A list of all the (DEFPACKAGE ) (IN-PACKAGE ..) (EXPORT ..)
top-level forms in the stream,
returned in the same order as they are read."
  (let ((interests '(cl:defpackage cl:in-package cl:export))
        (result))
      (loop
        (let ((form (read input-stream nil)))
          (unless form (return))
          (when (member (first form) interests :test #'eq)
            (push form result))))
    (nreverse result)))

(defun package-related-forms-of (package-lisp-files)
  (apply #'append
         (mapcar (lambda (lisp-file)
                   (with-open-file (in lisp-file
                                       :direction :input
                                       :element-type *utf-8-compatible-character-type*
                                       :external-format *utf-8-external-format*)
                     (package-related-forms in)))
                 package-lisp-files)))

;;; Try:
;;
;; (package-related-forms-of (list (asdf:system-relative-pathname '#:cl+ssl "src/package.lisp")))
;;
;; or
;;
;; (ql:quickload :cleric)
;; (package-related-forms-of (list (asdf:system-relative-pathname '#:cleric "src/packages.lisp")))
;;
;; or
;;
;; # find files with more than two defpackge forms:
;; find ~/quicklisp/dists/quicklisp/software/ -name '*.lisp' -exec sh -c 'test $(grep defpackage {} | wc -l) -gt 2' \; -print
;;
;; (package-related-forms-of (list "/home/anton/quicklisp/dists/quicklisp/software/cleric-20220220-git/src/packages.lisp"))

(defun all-exported (defpackage-form)
  (let* ((options (cddr defpackage-form))
         (export-options (remove-if-not (lambda (option)
                                          (eq :export (first option)))
                                        options)))
    
    (apply #'nconc (mapcar #'cdr export-options))
    ;(mapcar #'cdr export-options)
    ))

(assert (equal (mapcar #'string' (#:x #:y #:z #:a #:b #:c))
               (mapcar #'string
                       (with-input-from-string (s "(defpackage #:a
                                                     (:export #:x #:y #:z)
                                                     (:export #:a #:b #:c))")
                         (all-exported
                          (first (package-related-forms s)))))))

(defun as-list (val)
  (if (listp val) val (list val)))

(assert (equal '(1) (as-list 1)))
(assert (equal '(2) (as-list '(2))))

(defun unquote (form)
  (if (and (listp form)
           (eq 'quote (first form)))
      (second form)
      form))

(assert (equal '(1 2 3) (unquote '(quote (1 2 3)))))
(assert (equal '(a b c)
               (unquote (second (read-from-string "(export '(a b c))")))))
(assert (equal 'x (unquote (second (read-from-string "(export 'x)")))))

(defun combine-exports (defpackage-form package-related-forms)
  "Returns a list of symbols in the :EXPORT options
of the DEFPACKAGE-FORM combined with all the symbols
in explict top-level calls to function (EXPORT ...)
for the same package, where package is explicitly
specified as the 2nd parameter to EXPORT or
is seen in the nearest preceeding (IN-PACKAGE ...).
The symbols returned in the same order as they are
found in the forms."
  (let ((defpackage-name (second defpackage-form))
        (cur-package)
        (separate-exported))
    (dolist (form package-related-forms)
      (when (listp form)
        (when (eq 'cl:in-package (first form))
          (setq cur-package (second form)))
        (when (eq 'cl:export (first form))
          (let ((package (or (third form) cur-package)))
            (when (string= defpackage-name package)
              (push (as-list (unquote (second form)))
                    separate-exported))))))
    (apply #'nconc
           (all-exported defpackage-form)
           (nreverse separate-exported))))
#|
 (let* ((file (asdf:system-relative-pathname '#:cl+ssl "src/package.lisp"))
        (forms (package-related-forms-of (list file)))
        (defpackage-form (find 'cl:defpackage forms :key #'first)))
    (combine-exports defpackage-form forms))
|#

(assert (equal
         (mapcar #'string
                 '(#:a #:b #:c #:d #:e #:f #:g #:h #:x #:y #:z))
         (mapcar #'string
                 (combine-exports '(defpackage #:pkg
                                    (:export #:a #:b)
                                    (:export #:c #:d #:e))
                                  '((defpackage #:pkg
                                      (:export #:a #:b)
                                      (:export #:c #:d #:e))
                                    (export '(#:f #:g) #:pkg)
                                    ;; read-time value instead of simply in-package,
                                    ;; because otherwise, if we simply use in-package,
                                    ;; Slime is confused - it thinks that's a real
                                    ;; in-package and tries to use that packge
                                    ;; for evaluation in the rest of the file
                                    (#.'cl:in-package #:other)
                                    (export '(#:other-a #:other-b))
                                    (#.'cl:in-package #:pkg)
                                    (export '#:h)
                                    (export '(#:x #:y #:z))
                                    (export '(#:other-x #:other-y) #:other))))))

(defun md-escape (str)
  "Escapes markdown special characters"
  (let ((specials ;; "\\`*_{}[]()#+-.!" ; the dash symbol causes too much escaping
                  "\\`*_{}[]()#+.!" 
                  ))
    (with-output-to-string (out)
      (with-input-from-string (in str)
        (loop
          (let ((char (read-char in nil)))
            (unless char (return))
            (when (find char specials)
              (princ #\\ out))
            (princ char out)))))))

(assert (string= "\\*hello\\!\\*" (md-escape "*hello!*")))

(defun deduplicate-foreign (doc-nodes)
  "Deletes docparser:function-node and docparser:macro-node
if docparser:cffi-function is present,
because they duplicate each other (asked why in this ticket
https://github.com/eudoxia0/docparser/issues/30).
Desctructive - can modify the DOC-NODES."
  (if (find 'docparser:cffi-function doc-nodes :key 'type-of)
      (delete 'docparser:macro-node
              (delete 'docparser:function-node doc-nodes :key 'type-of)
              :key 'type-of)
      doc-nodes))

(defun node-name-str (docparser-node)
  (let ((name-str (string-downcase
                   (symbol-name (docparser:node-name docparser-node)))))
    (cond ((and (typep docparser-node 'docparser:operator-node)
                 (docparser:operator-setf-p docparser-node))
           (format nil "(setf ~A)" name-str))
          ((typep docparser-node 'docparser:cffi-function)
           (format nil "(~A ~S)"
                   name-str
                   (docparser:cffi-function-foreign-name docparser-node)))
          (t name-str))))



;;; Workaround for https://github.com/eudoxia0/docparser/issues/32
;;; Adds class slots and their accessors / readers / writers to the index.

(defclass accessor-node (docparser:operator-node) ())
(defclass reader-node (docparser:operator-node) ())
(defclass writer-node (docparser:operator-node) ())

(defun add-slots (docparser-index)
  (docparser:do-packages (pkg docparser-index)
    (docparser:do-nodes (node pkg)
      (when (typep node 'docparser:class-node)
        (dolist (slot (docparser:record-slots node))
          (docparser::add-node docparser-index slot)
          (dolist (reader (docparser:slot-readers slot))
            (docparser::add-node docparser-index
                                 (make-instance 'reader-node
                                                :lambda-list `((,(docparser:node-name node)
                                                                ,(docparser:node-name node)))
                                                :docstring (docparser:node-docstring slot)
                                                :name reader)))
          (dolist (writer (docparser:slot-writers slot))
            (docparser::add-node docparser-index
                                 (make-instance 'writer-node
                                                :lambda-list `(new-val
                                                               (,(docparser:node-name node)
                                                                ,(docparser:node-name node)))
                                                :docstring (docparser:node-docstring slot)
                                                :name writer)))
          (dolist (accessor (docparser:slot-accessors slot))
            ;; TODO: maybe jsut generate a reader-node
            ;;       and a writer-node for `(setf ,accessor)` ?
            (docparser::add-node docparser-index
                                 (make-instance 'accessor-node
                                                :lambda-list '()
                                                :docstring (docparser:node-docstring slot)
                                                :name accessor)))))))
  docparser-index)

(defun parse-docs (system-or-list)
  (add-slots (docparser:parse system-or-list)))

(defun node-type-str (docparser-node)
  (typecase docparser-node
    (docparser:cffi-function "CFFI function")
    (docparser:function-node "Function") ;; base class for the cffi-function
    (docparser:macro-node "Macro")
    (docparser:generic-function-node "Generic function")
    (docparser:method-node "Method")
    (writer-node "Writer")
    (reader-node "Reader")
    (docparser:operator-node "Operator") ;; base class of all the above
    (docparser:variable-node "Variable")
    (docparser:condition-node "Condition")
    (docparser:class-node "Class")
    (t (error "Unimplemented for type ~A (~A), extend the implementation.
 (See the print.lisp in the docparser library for some examples)"
              (type-of docparser-node) docparser-node))))

(defun node-header-str (docparser-node)
  (format nil "~A ~A"
          (node-type-str docparser-node)
          (node-name-str docparser-node)))

(defun node-lambda-list-str (operator-node)
  (let ((*package* (symbol-package (docparser:node-name operator-node)))
        (lambda-list (docparser:operator-lambda-list operator-node)))
    (if (null lambda-list)
        "()"
        (string-downcase
         (with-output-to-string (out)
           (prin1 lambda-list  out)
           (when (typep operator-node 'docparser:cffi-function)
             (princ " => " out)
             (prin1 (docparser:cffi-function-return-type operator-node)
                    out)))))))

(defun defpackage-documentation (defpackage-form)
  (let* ((options (cddr defpackage-form)))
    (second (assoc :documentation options :test #'eq))))

(assert (string= "test doc"
                 (defpackage-documentation '(defpackage #:somepkg
                                             (:use #:common-lisp)
                                             (:documentation "test doc")
                                             (:export #:func-1 #:func-2)))))

;; We use markdown intermediate representaiton
;; because initially it was the idea - geterate
;; markdown file which can be read at github.
;; Keeping that for the case if I want markdown
;; someday.
(defun markdown-package-docs (docparser-docs lisp-files
                              &key (with-headers nil)
                                package-filter
                                doc-node-filter)
  (let ((package-related-forms (package-related-forms-of lisp-files)))
    (with-output-to-string (out)
      (dolist (form package-related-forms)
        (when (eq 'cl:defpackage (first form))
          (let* ((defpackage-form form)
                 (package-name (second defpackage-form))
                 (package-doc (defpackage-documentation defpackage-form)))
            (unless (and package-filter (not (funcall package-filter package-name)))
              (format out "# Package ~A~%~%" package-name)
              (when package-doc
                ;; TODO: escape triple backquotes
                ;;       inside the package-doc
                (format out "~%```~%~A~%```~%~%" package-doc))
              (dolist (symbol (combine-exports defpackage-form
                                               package-related-forms))
                (let* ((symbol-docs (docparser:query docparser-docs
                                                     :package-name package-name
                                                     :symbol-name symbol))
                       (deduplicated-docs (deduplicate-foreign symbol-docs))
                       (filtered-docs (if doc-node-filter
                                          (remove-if-not doc-node-filter deduplicated-docs)
                                          deduplicated-docs)))
                  (when (zerop (length symbol-docs))
                    (cerror "Continue - proceed for the other symbols"
                            "docparser got no definition / documentation for the symbol ~S" symbol))
                  (unless (zerop (length filtered-docs))
                    (format out "~%___~%") ; horizontal rule
                    (loop for node across filtered-docs
                          do (let ((docstring (docparser:node-docstring node)))
                               (if with-headers
                                   (format out "~%#### ~A~%~%" (md-escape (node-header-str node)))
                                   (format out "~%~A ___~A___ "
                                           (md-escape (node-type-str node))
                                           (md-escape (node-name-str node))))
                               (when (typep node 'docparser:operator-node)
                                 (format out "_~A_"
                                         (md-escape (node-lambda-list-str node))))
                               (format out "~%")
                               (when docstring
                                 (format out "~%```~%~A~%```~%"
                                         ;; TODO: escape triple backquotes
                                         ;;       inside the docstring
                                         docstring))))))))))))))

(defun cur-timestamp-str ()
  (multiple-value-bind
        (second minute hour day month year day-of-week dst-p tz)
      (decode-universal-time (get-universal-time))
    (declare (ignore day-of-week))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d (GMT~@d~@[, DST~])"
            year
	    month
	    day
	    hour
	    minute
	    second
	    (- tz)
            dst-p)))

(defun render-package-html (docparser-docs lisp-files
                            &key
                              output-file
                              doc-node-filter
                              package-filter)
  (unless output-file
    (setq output-file (make-pathname :type "html"
                                     :defaults (first lisp-files))))
  (with-open-file (out output-file
                       :direction :output
                       :element-type *utf-8-compatible-character-type*
                       :external-format *utf-8-external-format*
                       :if-exists :supersede)
    (let ((3bmd-code-blocks:*code-blocks* t)
          (3bmd-code-blocks:*code-blocks-default-colorize* nil))
      (3bmd:parse-string-and-print-to-stream
       (markdown-package-docs docparser-docs
                              lisp-files
                              :doc-node-filter doc-node-filter
                              :package-filter package-filter)
       out))
    (format out "<hr/>~%")
    (format out "Generated with package-doc-dump, ~A~%" (cur-timestamp-str)))
  output-file)

(defun ensure-list (val)
  (if (listp val)
      val
      (list val)))

(defun dump-html (system-or-list
                  package-defining-lisp-file/s
                  &key output-file
                    doc-node-filter
                    package-filter)
  (render-package-html (parse-docs system-or-list)
                       (ensure-list package-defining-lisp-file/s)
                       :output-file output-file
                       :doc-node-filter doc-node-filter
                       :package-filter package-filter))

#| TODO:

+ add a timestimp to the resulting doc
- wrap the result into <html><body> </body></html> and other tags (title, charset, lang)?
+ escape markdown (at least stars in variable names)
+ print empty lambda lists as () instead of NIL
+ deduplicate `function` and `foreing-function`
+ class slot readers are not documented by docparser:parse
+ class slot accessors / writers / readers need more attentions, ideally:
  - no labmda list for accessor currently
  - maybe specify `:setf-p` for writers?
  - maybe instead of an accessor node generate a pair of nodes:
    - Reader `accessor-name ((obj class-name))`
    - Writer `(setf accessor-name) (new-val (obj class-name)`
  - inherit accessor / writer / reader noes from generic-function-node?
    or from method node? Or generate both generic function and method? 
  - is that helpful that writer lambda list is 
           writer-name (new-val ((obj class-name))
    maybe better
           (setf (writer-name obj) new-val)?
    The second variant assumes writer was defined as `:writer (setf writer-name)`
    but that's not always true - CL allows just `:writer writer-name`
    which is not suitable for setf.
  - All above should be solved in a way consistend with 
    SETF handling by docparser of other node types (functions, generic functions)
+ remove the "Method stream-fd ((stream ccl::basic-stream))"
+ Render the SETF functions properly.
+ multiple :export clauses
- include the symbols absent in the :export clauses but exported programmatically?
+ multiple pakcages in one file
- init value for variables?
- symbols defined in upper case should be documented with upper case names?
- Remove exceecive escaping from the markdown.
  Partially done by not escaping the '-' char, but that
  may be not always right thing to do. Review again.
- Known issue: it seems that, due to how docparser:parse works,
  if the packages being documented re-expert
  sybmold from other asdf systems, which where
  already loaded at the moment of docparser:parse
  invocation, those symbols will remain undocumented.
  Be sure to unload all such systems, or just work
  in a fresh lisp session.
  (Actually evern unloading and removing the fasls from ~/.cache/ 
  then restarting SLIME does not help - what's going on?)


|#
