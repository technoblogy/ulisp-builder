;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

;; Generate *********************************************************************************************

(defun write-no-comments (stream string comments)
  (cond
   (comments
    (write-string string stream)
    (terpri stream))
   (t
    (let ((start 0))
      (loop
       (let* ((com (search "/*" string :start2 start))
              (ment (when com (search "*/" string :start2 com))))
         (cond
          ((and com ment (char= (char string (+ com 2)) #\newline)) (write-string string stream :start start :end com) ; 32
           (setq start (+ ment 3))) ; Swallow return too
          (t (write-string string stream :start start)
             (terpri stream)
             (return)))))))))

(defun definition-p (string)
  (cond
   ((null string) nil)
   ((stringp string)
       (let* ((com (search "/*" string :start2 0))
              (ment (when com (search "*/" string :start2 com))))
         (not (and com ment (= com 1) (= ment (- (length string) 2))))))
   (t t)))

(defun mappend (fn &rest lsts)
  "maps elements in list and finally appends all resulted lists."
  (apply #'append (apply #'mapcar fn lsts)))

;; (wildcards (if wildcard (reduce #'+ (map 'list #'(lambda (x) (1- (length x))) (cadar keywords))) 0))

#|
(defun do-keyword-enums (str keywords)
  (let* ((wildcard (null (caar keywords)))
         (only-wildcard (and wildcard (null (cdr keywords)))))
    (dotimes (n (length keywords))
      (destructuring-bind (cpu lists) (nth n keywords)
        (let ((klist (mappend #'(lambda (x) (map 'list #'(lambda (y) (if (listp y) (car y) y)) (cdr x))) lists)))
          (unless (and wildcard (zerop n)) (format str "#~[~:;el~]if defined(~a)~%" (if wildcard (1- n) n) cpu))
          (format str "~{~a~%~}" (split-into-lines (format nil "~{K_~a,~^ ~}" klist))))))
    (unless only-wildcard (format str "#endif~%"))))
|#

(defun do-keyword-progmems (str keywords i)
  (let* ((wildcard (null (caar keywords)))
         (only-wildcard (and wildcard (null (cdr keywords))))
         (j i))
    (dotimes (n (length keywords))
      (destructuring-bind (cpu lists) (nth n keywords)
        (let ((klist (mappend #'(lambda (x) (cdr x)) lists)))
          (when cpu 
            (setq j i)
            (format str "#~[~:;el~]if defined(~a)~%" (if wildcard (1- n) n) cpu))
          (dolist (k klist)
            (format str "const char string~a[] PROGMEM = \":~a\";~%" j 
                    (substitute #\- #\_ (string-downcase (if (consp k) (car k) k))))
            (incf j))
          #|(when cpu (format str "const char string~a[] PROGMEM = \"\";~%" j))|#)
        (unless cpu (setq i j))))
    (if only-wildcard nil #|(format str "const char string~a[] PROGMEM = \"\";~%" j)|#
      (format str "#endif~%"))))

(defun needs-&-prefix (a b)
  (or
   (and (eq a 'register) (listp b) (stringp (second b)) (char/= (char (second b) 0) #\())
   (and (eq a 'register) (atom b))))

(defun docstring (definition enum string)
  (cond
   ((null definition) nil)
   ((stringp definition)
    (let* ((com (search "/*" definition :start2 0))
           (ment (when com (search "*/" definition :start2 com))))
      (when (and com ment) (subseq definition (+ com 3) (- ment 1)))))
   ((keywordp definition) nil)
   ((symbolp definition)
    (let* ((definition (with-output-to-string (str) (funcall definition str enum string t)))
           (com (search "/*" definition :start2 0))
           (ment (when com (search "*/" definition :start2 com))))
      (when (and com ment) (subseq definition (+ com 3) (- ment 1)))))
   (t nil)))

(defun replace-linebreaks (string)
  (let ((result "")
        (start 0))
    (loop
     (let ((cr (position #\newline string :start start)))
       (when (not cr) (return (concatenate 'string result (string-trim '(#\space) (subseq string start)))))
       (setq result 
             (concatenate 'string result (string-trim '(#\space) (subseq string start cr)) "\\n\"" (string #\newline) "\""))
       (setq start (+ 1 cr))))))

(defun do-keyword-table (str keywords i documentation)
  (let* ((wildcard (null (caar keywords)))
         (only-wildcard (and wildcard (null (cdr keywords))))
         (docstring nil)
         (j i))
    (dotimes (n (length keywords))
      (destructuring-bind (cpu lists) (nth n keywords)
        (let ((klist (mappend #'(lambda (x) (mapcar #'(lambda (y) (cons (car x) y)) (cdr x))) lists)))
          (when cpu
            (setq j i)
            (format str "#~[~:;el~]if defined(~a)~%" (if wildcard (1- n) n) cpu))
          (dolist (k klist)
            (destructuring-bind (a . b) k
              (if documentation
                  (format str "  { string~a, (fn_ptr_type)~:[~;&~]~a, ~a, ~:[NULL~;doc~a~] },~%"
                          j (needs-&-prefix a b) (if (listp b) (second b) b) (or a 0) docstring j)
                (format str "  { string~a, (fn_ptr_type)~:[~;&~]~a, ~a },~%"
                        j (needs-&-prefix a b) (if (listp b) (second b) b) (or a 0)))
              (incf j))))
        (unless cpu (setq i j))))
    (if only-wildcard nil
      (format str "#endif~%"))))

(defparameter *enums* '(NIL TEE NOTHING OPTIONAL INITIALELEMENT ELEMENTTYPE BIT AMPREST LAMBDA LET
                            LETSTAR CLOSURE PSTAR QUOTE CAR FIRST CDR REST NTH DEFUN DEFVAR DEFCODE AREF STRINGFN FORMAT
                            PINMODE DIGITALWRITE ANALOGREAD REGISTER ANALOGREFERENCE))

(defun build (&optional (platform :avr) (comments nil) (documentation (find :doc *features*)))
  (let* ((maxsymbol 0)
         (definitions *definitions*)
         (keywords (eval (intern (format nil "*KEYWORDS-~a*" platform) :cl-user))))
    (flet ((include (section str)
             (let ((special (intern (format nil "*~a-~a*" section platform) :cl-user))
                   (default (intern (format nil "*~a*" section) :cl-user)))
               (cond
                ((boundp special) 
                 (let ((inc (eval special)))
                   (cond
                    ((listp inc) (map nil #'(lambda (x) (write-no-comments str x comments)) inc))
                    (t (write-no-comments str inc comments)))))
                ((boundp default) 
                 (let ((inc (eval default)))
                   (cond
                    ((listp inc) (map nil #'(lambda (x) (write-no-comments str x comments)) inc))
                    (t (write-no-comments str inc comments)))))
                (t nil)))))
      ;;           
      (with-open-file (str (capi:prompt-for-file "Output File" :operation :save :pathname "/Users/david/Desktop/") :direction :output)
        ;; Write preamble
    ; (include :header str)
        (format str (eval (intern (format nil "*~a-~a*" :title platform) :cl-user)) *release* *date*)
        (terpri str)
        (include :header str)
        (include :workspace str)
        (include :macros str)
        (include :constants str)
        (include :typedefs str)

        ;; Collect enums we need to use in the C functions
        (let (enumlist)
          (dolist (section definitions)
            (destructuring-bind (comment defs &optional prefix) section
              (declare (ignore comment prefix))
              (dolist (item defs)
                (destructuring-bind (enum string min max definition) item
                  (declare (ignore string min max definition))        
                  (when (member enum *enums*) (push enum enumlist))))))
 
          ;; Write enum declarations
          (let ((enums (split-into-lines (format nil "~{~a, ~}" (reverse enumlist)) 12)))
            (format str "~%enum builtins: builtin_t { ~{~a~%~} };~%" enums)))

        ;;
        (include :global-variables str)
        (include :error-handling str)
        (include :setup-workspace str)
        (include :make-objects str)
        ;; Write utilities
        (include :garbage-collection str)
        (include :compactimage str)
        (include :make-filename str)
        (include :saveimage str)
        (include :tracing str)
        (include :helper-functions str)
        (include :association-lists str)
        (include :array-utilities str)
        (include :string-utilities str)
        (include :closures str)
        (include :in-place str)
        (include :i2c-interface str)
        (include :stream-interface str)
    ; (include :watchdog str)
        (include :check-pins str)
        (include :note str)
        (include :sleep str)
        (include :prettyprint str)
        (include :assembler str)
        #+interrupts
        (include :interrupts str)

        ;; Write function definitions
        (dolist (section definitions)
          (destructuring-bind (comment defs &optional prefix) section
            (declare (ignore prefix))
            (when comment (format str "~%// ~a~%" comment))
            (dolist (item defs)
              (destructuring-bind (enum string min max definition) item
                (declare (ignore min max))
                (cond
                 ((null (definition-p definition)) nil)
                 ((stringp definition)
                  (write-no-comments str definition comments))
                 ((keywordp definition) nil)
                 ((symbolp definition)
                  (funcall definition str enum string comments)
                  (format str "~%"))
                 (t nil))))))

        ;; Write symbol names
        (format str "~%// Built-in symbol names~%")
        (let ((i 0))
          (dotimes (pass 2)
            (dolist (section definitions)
              (destructuring-bind (comment defs &optional prefix) section
                (declare (ignore comment prefix))
                (dolist (item defs)
                  (destructuring-bind (enum string min max definition) item
                    (declare (ignore definition min max))        
                    (when (eq (plusp pass) (not (member enum *enums*)))
                      (let ((lower (string-downcase enum)))
                        (format str "const char string~a[] PROGMEM = \"~a\";~%" i (or string lower))
                        (setq maxsymbol (max maxsymbol (length (or string lower))))
                        (incf i))))))))
          ;; Do keywords
          (do-keyword-progmems str keywords i))

        ;; Write documentation strings
        (when documentation
          (format str "~%// Documentation strings~%")
          (let ((i 0))
            (dotimes (pass 2)
              (dolist (section definitions)
                (destructuring-bind (comment defs &optional prefix) section
                  (declare (ignore comment prefix))
                  (dolist (item defs)
                    (destructuring-bind (enum string min max definition) item
                      (declare (ignore min max))
                      (when (eq (plusp pass) (not (member enum *enums*)))
                        (let ((docstring (docstring definition enum string)))
                          (when docstring 
                            (format str "const char doc~a[] PROGMEM = \"~a\";~%"
                                    i (replace-linebreaks docstring)))
                          (incf i))))))))))

        ;; Write table
        (format str "~%// Built-in symbol lookup table~%")
        (flet ((minmax (prefix min max)
                 (let ((pre (cond
                             ((string= prefix "fn") 2)
                             ((string= prefix "sp") 3)
                             ((string= prefix "tf") 1)
                             (t 0))))
                   (+ (ash pre 6) (ash min 3) (min max 7)))))
          (let ((i 0))
            (format str "const tbl_entry_t lookup_table[] PROGMEM = {~%")
            (dotimes (pass 2)
              (dolist (section definitions)
                (destructuring-bind (comment defs &optional (prefix "fn")) section
                  (declare (ignore comment))
                  (dolist (item defs)
                    (destructuring-bind (enum string min max definition) item
                      (when (eq (plusp pass) (not (member enum *enums*)))
                        (let ((docstring (docstring definition enum string))
                              (lower (cond
                                      ((consp definition) (string-downcase (car definition)))
                                      ((keywordp definition) definition)
                                      (t (string-downcase enum)))))
                          (if documentation
                              (format str "  { string~a, ~:[NULL~2*~;~a_~a~], 0~3,'0o, ~:[NULL~;doc~a~] },~%" 
                                      i (definition-p definition) prefix lower (minmax prefix min max) docstring i)
                            (format str "  { string~a, ~:[NULL~2*~;~a_~a~], 0~3,'0o },~%" 
                                    i (definition-p definition) prefix lower (minmax prefix min max)))
                          (incf i))))))))
      ; Do keywords
            (do-keyword-table str keywords i documentation)
            (format str "};~%")))

        ;; Write rest
        (include :table str)
        (include :eval str)
        (include :print-functions str)
        (include :read-functions str)
        (when (eq platform :badge) (write-string *lisp-badge* str))
        (include :setup1 str)
        (format str (eval (intern (format nil "*~a*" :setup2) :cl-user)) *release*)
        (terpri str)
        (include :repl str)
        (include :loop str)
        maxsymbol))))

        
        