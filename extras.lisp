;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

; To run do (generate)

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

#-float
(defun comparison (str enum string)
  (format str "
object *fn_~a (object *args, object *env) {
  (void) env;
  int arg1 = checkinteger(~a, first(args));
  args = cdr(args);
  while (args != NULL) {
    int arg2 = checkinteger(~a, first(args));
    if (!(arg1 ~a arg2)) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}" (string-downcase enum) 
          enum enum
          (cond
           ((string= string "=") "==")
           (t string))))

#+float
(defun comparison (str enum string)
  (declare (ignore string))
  (format str "
object *fn_~a (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg2 = first(args);
    if (integerp(arg1) && integerp(arg2)) {
      if (!((arg1->integer) ~a (arg2->integer))) return nil;
    } else if (!(checkintfloat(~a, arg1) ~a checkintfloat(~a, arg2))) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}" (string-downcase enum)
          (cond
           ((string= string "=") "==")
           (t string))
          enum
          (cond
           ((string= string "=") "==")
           (t string))
          enum))


(defun float-function (str enum string)
  (declare (ignore string))
  (format str "
object *fn_~a (object *args, object *env) {
  (void) env;
  return makefloat(~a(checkintfloat(~a, first(args))));
}" (string-downcase enum) (string-downcase enum) enum))

(defun truncate-function (str enum string)
  (declare (ignore string))
  (format str "
object *fn_~a (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  args = cdr(args);
  if (args != NULL) return number(~a(checkintfloat(~a, arg) / checkintfloat(~a, first(args))));
  else return number(~a(checkintfloat(~a, arg)));
}" (string-downcase enum) 
          (cdr (assoc enum '((CEILING . "ceil") (FLOOR . "floor") (TRUNCATE . "trunc") (ROUND . "round"))))
          enum enum
          (cdr (assoc enum '((CEILING . "ceil") (FLOOR . "floor") (TRUNCATE . "trunc") (ROUND . "round"))))
          enum))

(defun numeric1 (str enum string)
  (declare (ignore string))
  (format str "
object *fn_~a (object *args, object *env) {
  (void) env;
  int arg = checkinteger(~a, first(args));
  return number(~a(arg));
}" (string-downcase enum) enum (string-downcase enum)))

(defun bitwise (str enum string)
  (declare (ignore string))
  (format str "
object *fn_~a (object *args, object *env) {
  (void) env;
  int result = ~a;
  while (args != NULL) {
    result = result ~a checkinteger(~a, first(args));
    args = cdr(args);
  }
  return number(result);
}" (string-downcase enum)
          (cdr (assoc enum '((LOGAND . "-1") (LOGIOR . "0") (LOGXOR . "0"))))
          (cdr (assoc enum '((LOGAND . "&") (LOGIOR . "|") (LOGXOR . "^"))))
          enum))

; For max or min

(defun numeric2 (str enum string)
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

(defun cxr (str enum string)
  (declare (ignore string))
  (let ((lower (string-downcase enum)))
    (format str "
object *fn_~a (object *args, object *env) {
  (void) env;
  return ~{c~arx(~}first(args)~a;
}" lower (cdr (butlast (coerce lower 'list))) (make-string (- (length lower) 2) :initial-element #\)))))

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