;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

; To run do (build)

; Sharp-double-quote

(defun sharp-double-quote-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((prev (read-char stream) curr)
         (curr (read-char stream) (read-char stream)))
        ((and (char= prev #\") (char= curr #\#)))
      (push prev chars))
    (coerce (nreverse chars) 'string)))

(set-dispatch-macro-character
  #\# #\" #'sharp-double-quote-reader)

; Code generation functions

(defun float-function (str enum string comments)
  (declare (ignore string))
  (format str "
~:[~2*~;/*
  (~a number)
  Returns ~a(number).
*/
~]object *fn_~a (object *args, object *env) {
  (void) env;
  return makefloat(~a(checkintfloat(~a, first(args))));
}" 
          comments
          (string-downcase enum)
          (string-downcase enum)
          (string-downcase enum)
          (string-downcase enum) 
          enum))

(defun truncate-function (str enum string comments)
  (declare (ignore string))
  (format str "
~:[~2*~;/*
  (~a number [divisor])
  Returns ~a(number/divisor). If omitted, divisor is 1.
*/
~]object *fn_~a (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  args = cdr(args);
  if (args != NULL) return number(~a(checkintfloat(~a, arg) / checkintfloat(~a, first(args))));
  else return number(~a(checkintfloat(~a, arg)));
}" 
          comments
          (string-downcase enum)
          (cdr (assoc enum '((CEILING . "ceil") (FLOOR . "floor") (TRUNCATE . "trunc") (ROUND . "round"))))

          (string-downcase enum) 
          (cdr (assoc enum '((CEILING . "ceil") (FLOOR . "floor") (TRUNCATE . "trunc") (ROUND . "round"))))
          enum enum
          (cdr (assoc enum '((CEILING . "ceil") (FLOOR . "floor") (TRUNCATE . "trunc") (ROUND . "round"))))
          enum))

#|
(defun numeric1 (str enum string comments)
  (declare (ignore string))
  (format str "
object *fn_~a (object *args, object *env) {
  (void) env;
  int arg = checkinteger(~a, first(args));
  return number(~a(arg));
}" (string-downcase enum) enum (string-downcase enum)))
|#

(defun bitwise (str enum string comments)
  (declare (ignore string))
  (format str "
~:[~2*~;/*
  (~a [value*])
  Returns the bitwise ~a of the values.
*/
~]object *fn_~a (object *args, object *env) {
  (void) env;
  int result = ~a;
  while (args != NULL) {
    result = result ~a checkinteger(~a, first(args));
    args = cdr(args);
  }
  return number(result);
}"
          comments
          (string-downcase enum)
          (cdr (assoc enum '((LOGAND . "&") (LOGIOR . "|") (LOGXOR . "^"))))
          (string-downcase enum)
          (cdr (assoc enum '((LOGAND . "-1") (LOGIOR . "0") (LOGXOR . "0"))))
          (cdr (assoc enum '((LOGAND . "&") (LOGIOR . "|") (LOGXOR . "^"))))
          enum))

#|
; For max or min

(defun numeric2 (str enum string comments)
  (declare (ignore string))
  (format str "
object *fn_~a (object *args, object *env) {
  (void) env;
  int result = integer(first(args));
  args = cdr(args);
  while (args != NULL) {
    result = ~a(result,integer(car(args)));
    args = cdr(args);
  }
  return number(result);
}" (string-downcase enum) (string-downcase enum)))
|#

(defun split-into-lines (string &optional (indent 0))
  (let* ((linelen 106)
         (start 0)
         (end (- linelen indent)) 
         (length (length string))
         result)
    (loop
     (when (>= end length)
       (push (subseq string start) result) 
       (return (reverse result)))
     (let ((comma (position #\, string :start start :end end :from-end t)))
       (push (subseq string start (1+ comma)) result)
       (setq start (+ comma 2) end (+ comma 2 linelen))))))