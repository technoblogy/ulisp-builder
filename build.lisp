;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

;; Generate *********************************************************************************************

; PSTR(), PGM_P, and PROGRAM are unnecessary on ARM.
(defun process-text (stream string platform comments)
  (when (or (eq platform :arm) (eq platform :esp) (eq platform :riscv))
    (setq string (strip-pstr string))
    (setq string (global-replace "PGM_P " "const char *" string))
    (setq string (global-replace "PROGMEM " "" string)))
  (write-no-comments stream string comments))

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

(defun strip-pstr (string)
  (let ((start 0) (result ""))
    (loop
     (let* ((pstr (search "PSTR(\"" string :start2 start))
            (term (when pstr (search "\")" string :start2 (+ pstr 6)))))
       (cond
        ((and pstr term)
         (setq result (concatenate 'string result (subseq string start pstr) (subseq string (+ pstr 5) (+ term 1))))
         (setq start (+ term 2)))
        (t
         (setq result (concatenate 'string result (subseq string start)))
         (return result)))))))

(defun global-replace (target replace string)
  (let ((start 0) (result "") (span (length target)))
    (loop
     (let ((pgmp (search target string :start2 start)))
       (cond
        (pgmp
         (setq result (concatenate 'string result (subseq string start pgmp) replace))
         (setq start (+ pgmp span)))
        (t
         (setq result (concatenate 'string result (subseq string start)))
         (return result)))))))

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

(defun do-keyword-progmems (str keywords i progmem)
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
            (format str "const char string~a[] ~a= \":~a\";~%" j progmem
                    (substitute #\- #\_ (string-downcase (if (consp k) (car k) k))))
            (incf j)))
        (unless cpu (setq i j))))
    (if only-wildcard nil (format str "#endif~%"))))

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

(defparameter *enums* '(NIL TEE NOTHING OPTIONAL FEATURES INITIALELEMENT ELEMENTTYPE TEST BIT AMPREST LAMBDA LET
                            LETSTAR CLOSURE PSTAR QUOTE CAR FIRST CDR REST NTH EQ CHAR DEFUN DEFVAR DEFCODE AREF STRINGFN FORMAT
                            PINMODE DIGITALWRITE ANALOGREAD REGISTER ANALOGREFERENCE))

(defun build (&optional (platform :avr) (comments nil) (documentation (find :doc *features*)))
  (let* ((maxsymbol 0)
         (definitions *definitions*)
         (progmem (if (or (eq platform :arm) (eq platform :esp) (eq platform :riscv)) "" "PROGMEM "))
         (keywords (eval (intern (format nil "*KEYWORDS-~a*" platform) :cl-user))))
    (flet ((include (section str)
             (let ((special (intern (format nil "*~a-~a*" section platform) :cl-user))
                   (default (intern (format nil "*~a*" section) :cl-user)))
               (cond
                ((boundp special) 
                 (let ((inc (eval special)))
                   (cond
                    ((listp inc) (map nil #'(lambda (x) (process-text str x platform comments)) inc))
                    (t (process-text str inc platform comments)))))
                ((boundp default) 
                 (let ((inc (eval default)))
                   (cond
                    ((listp inc) (map nil #'(lambda (x) (process-text str x platform comments)) inc))
                    (t (process-text str inc platform comments)))))
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
        (include :feature-list str)
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
                  (process-text str definition platform comments))
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
                        (format str "const char string~a[] ~a= \"~a\";~%" i progmem (or string lower))
                        (setq maxsymbol (max maxsymbol (length (or string lower))))
                        (incf i))))))))
          ;; Do keywords
          (do-keyword-progmems str keywords i progmem))

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
                            (format str "const char doc~a[] ~a= \"~a\";~%" i progmem (replace-linebreaks docstring)))
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
            (format str "const tbl_entry_t lookup_table[] ~a= {~%" progmem)
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

        
        