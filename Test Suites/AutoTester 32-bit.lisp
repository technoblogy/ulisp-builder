; uLisp Auto Tester

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

; do (run-tests)

;;; ================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "serial-port"))

(cl:in-package "CL-USER")

;;; ================================================================
;;; Class SERIAL-STREAM

(defclass serial-stream (stream:fundamental-character-input-stream
                         stream:fundamental-character-output-stream)
  ((serial-port :initform nil 
                :initarg :serial-port
                :accessor stream-serial-port)))

(defmethod initialize-instance :after ((stream serial-stream)
                                       &key name (baud-rate 9600) (data-bits 8) (stop-bits 1) (parity :none)
                                       &allow-other-keys)
  (unless (stream-serial-port stream)
    (check-type name string)
    (setf (stream-serial-port stream)
          (serial-port:open-serial-port name
                                        :baud-rate baud-rate
                                        :data-bits data-bits
                                        :stop-bits stop-bits
                                        :parity    parity))))

(defmethod stream-element-type ((stream serial-stream))
  'character)

(defmethod input-stream-p ((stream serial-stream))
  t)

(defmethod output-stream-p ((stream serial-stream))
  t)

;;; ================================================================
;;; Input

(defmethod stream:stream-read-char ((stream serial-stream))
  (serial-port:read-serial-port-char (stream-serial-port stream)))

(defmethod stream:stream-read-char-no-hang ((stream serial-stream))
  (when (stream:stream-listen stream)
    (stream:stream-read-char stream)))

(defmethod stream:stream-listen ((stream serial-stream))
  (serial-port:serial-port-input-available-p (stream-serial-port stream)))

(defmethod stream:stream-clear-input ((stream serial-stream))
  (loop while (stream:stream-listen stream)
        do (stream:stream-read-char stream))
  nil)


;;; ================================================================
;;; Output

(defmethod stream:stream-write-char ((stream serial-stream) char)
  (serial-port:write-serial-port-char char (stream-serial-port stream)))

(defmethod stream:stream-write-string ((stream serial-stream) string &optional (start 0) (end (length string)))
  (serial-port:write-serial-port-string string (stream-serial-port stream) t :start start :end end))

(defmethod stream:stream-force-output ((stream serial-stream))
  nil)

(defmethod stream:stream-finish-output ((stream serial-stream))
  nil)

(defmethod stream:stream-clear-output ((stream serial-stream))
  nil)

(defmethod close :after ((stream serial-stream) &key abort)
  (declare (ignorable abort))
  (serial-port:close-serial-port (stream-serial-port stream)))

;;; ================================================================
;;; Example

(defparameter *tests*

#"#| Arithmetic |#

(aeq '* 9 (* -3 -3))
(aeq '* 32580 (* 180 181))
(aeq '* 1 (*))
(aeq '+ 32767 (+ 32765 1 1))
(aeq '+ 0 (+))
(aeq '+ -2 (+ -1 -1))
(aeq '- -4 (- 4))
(aeq '- 0 (- 4 2 1 1))
(aeq '/ 2 (/ 60 10 3))
(aeq '1+ 2 (1+ 1))
(aeq '1+ 0 (1+ -1))
(aeq '1- 0 (1- 1))

#| Comparisons |#

(aeq '< t (< -32768 32767))
(aeq '< t (< -1 0))
(aeq '< t (< 1 2 3))
(aeq '< t (< 1))
(aeq '< nil (< 1 3 2))
(aeq '< nil (< -1 -2))
(aeq '< nil (< 10 10))
(aeq '<= t (<= 10 10))
(aeq '= t (= 32767 32767))
(aeq '>= t (>= 10 10))
(aeq '>= nil (>= 9 10))
(aeq '/= t (/= 1))
(aeq '/= nil (/= 1 2 1))
(aeq '/= nil (/= 1 2 3 1))
(aeq '/= t (/= 1 2 3 4))
(aeq 'plusp t (plusp 1))
(aeq 'plusp nil (plusp 0))
(aeq 'plusp nil (plusp -1))
(aeq 'minusp nil (minusp 1))
(aeq 'minusp nil (minusp 0))
(aeq 'minusp t (minusp -1))
(aeq 'zerop nil (zerop 1))
(aeq 'zerop t (zerop 0))
(aeq 'zerop nil (zerop -1))
(aeq 'evenp nil (evenp 1))
(aeq 'evenp t (evenp 0))
(aeq 'evenp nil (evenp -1))
(aeq 'oddp t (oddp 1))
(aeq 'oddp nil (oddp 0))
(aeq 'oddp t (oddp -1))

#| Maths functions |#

(aeq 'abs 10 (abs 10))
(aeq 'abs 10 (abs -10))
(aeq 'max 45 (max 23 45))
(aeq 'max -23 (max -23 -45))
(aeq 'min 23 (min 23 45))
(aeq 'min -45 (min -23 -45))
(aeq 'zerop t (zerop 0))
(aeq 'zerop nil (zerop 32767))
(aeq 'mod 1 (mod 13 4))
(aeq 'mod 3 (mod -13 4))
(aeq 'mod -3 (mod 13 -4))
(aeq 'mod -1 (mod -13 -4))

#| Number entry |#

(aeq 'hex -1 #xFFFFFFFF)
(aeq 'hex 1 #x0001)
(aeq 'hex 4112 #x1010)
(aeq 'oct 511 #o777)
(aeq 'oct 1 #o1)
(aeq 'oct 65535 #o177777)
(aeq 'bin -1 #b11111111111111111111111111111111)
(aeq 'bin 10 #b1010)
(aeq 'bin 0 #b0)
(aeq 'hash 12 #'12)
(aeq 'hash 6 (funcall #'(lambda (x) (+ x 2)) 4))

#| Boolean |#

(aeq 'and 7 (and t t 7))
(aeq 'and nil (and t nil 7))
(aeq 'or t (or t nil 7))
(aeq 'or 1 (or 1 2 3))
(aeq 'or nil (or nil nil nil))
(aeq 'or 'a (or 'a 'b 'c))
(aeq 'or 1 (let ((x 0)) (or (incf x)) x))

#| Bitwise |#

(aeq 'logand -1 (logand))
(aeq 'logand 170 (logand #xAA))
(aeq 'logand 0 (logand #xAAAA #x5555))
(aeq 'logior 0 (logior))
(aeq 'logior 170 (logior #xAA))
(aeq 'logior #xFFFF (logior #xAAAA #x5555))
(aeq 'logxor 0 (logxor))
(aeq 'logxor 170 (logior #xAA))
(aeq 'logxor 255 (logxor #xAAAA #xAA55))
(aeq 'lognot -43691 (lognot #xAAAA))
(aeq 'ash 492 (ash 123 2))
(aeq 'ash 65535 (ash #xFFFF 0))
(aeq 'ash 16383 (ash #xFFFF -2))
(aeq 'ash 262140 (ash #xFFFF 2))
(aeq 'ash 8191 (ash #x7FFF -2))
(aeq 'logbitp t (logbitp 0 1))
(aeq 'logbitp t (logbitp 1000 -1))
(aeq 'logbitp nil (logbitp 1000 0))

#| Tests |#

(aeq 'atom t (atom nil))
(aeq 'atom t (atom t))
(aeq 'atom nil (atom '(1 2)))
(aeq 'consp nil (consp 'b))
(aeq 'consp t (consp '(a b)))
(aeq 'consp nil (consp nil))
(aeq 'listp nil (listp 'b))
(aeq 'listp t (listp '(a b)))
(aeq 'listp t (listp nil))
(aeq 'numberp t (numberp (+ 1 2)))
(aeq 'numberp nil (numberp 'b))
(aeq 'numberp nil (numberp nil))
(aeq 'symbolp t (symbolp 'b))
(aeq 'symbolp nil (symbolp 3))
(aeq 'symbolp t (symbolp nil))
(aeq 'streamp nil (streamp 'b))
(aeq 'streamp nil (streamp nil))
(aeq 'boundp t (let (x) (boundp 'x)))
(aeq 'boundp nil (let (x) (boundp 'y)))

#| cxr operations |#

(aeq 'car 'a (car '(a b c)))
(aeq 'car nil (car nil))
(aeq 'first 'a (first '(a b c)))
(aeq 'first nil (first nil))
(aeq 'cdr 'b (cdr '(a . b)))
(aeq 'cdr 'b (car (cdr '(a b))))
(aeq 'cdr nil (cdr nil))
(aeq 'rest 'b (rest '(a . b)))
(aeq 'rest 'b (car (rest '(a b))))
(aeq 'rest nil (rest nil))
(aeq 'caaar 'a (caaar '(((a)))))
(aeq 'caaar 'nil (caaar nil))
(aeq 'caadr 'b (caadr '(a (b))))
(aeq 'caadr 'nil (caadr nil))
(aeq 'caar 'a (caar '((a))))
(aeq 'caar 'nil (caar nil))
(aeq 'cadar 'c (cadar '((a c) (b))))
(aeq 'cadar 'nil (cadar nil))
(aeq 'caddr 'c (caddr '(a b c)))
(aeq 'caddr 'nil (caddr nil))
(aeq 'cadr 'b (cadr '(a b)))
(aeq 'second 'nil (second '(a)))
(aeq 'second 'b (second '(a b)))
(aeq 'cadr 'nil (cadr '(a)))
(aeq 'caddr 'c (caddr '(a b c)))
(aeq 'caddr 'nil (caddr nil))
(aeq 'third 'c (third '(a b c)))
(aeq 'third 'nil (third nil))
(aeq 'cdaar 'b (car (cdaar '(((a b)) b c))))
(aeq 'cdaar 'nil (cdaar nil))
(aeq 'cdadr 'c (car (cdadr '(a (b c)))))
(aeq 'cdadr 'nil (cdadr nil))
(aeq 'cdar 'b (car (cdar '((a b c)))))
(aeq 'cdar 'nil (cdar nil))
(aeq 'cddar 'c (car (cddar '((a b c)))))
(aeq 'cddar 'nil (cddar nil))
(aeq 'cdddr 'd (car (cdddr '(a b c d))))
(aeq 'cdddr nil (car (cdddr '(a b c))))
(aeq 'cddr 'c (car (cddr '(a b c))))
(aeq 'cddr 'nil (cddr '(a)))

#| List operations |#

(aeq 'cons 'a (car (cons 'a 'b)))
(aeq 'cons nil (car (cons nil 'b)))
(aeq 'append 6 (length (append '(a b c) '(d e f))))
(aeq 'append nil (append nil nil))
(aeq 'append '(1 2 3 4 5 . 6) (append '(1 2 3) '(4 5 . 6)))
(aeq 'list nil (car (list nil)))
(aeq 'list 'a (car (list 'a 'b 'c)))
(aeq 'reverse 'c (car (reverse '(a b c))))
(aeq 'reverse nil (reverse nil))
(aeq 'length 0 (length nil))
(aeq 'length 4 (length '(a b c d)))
(aeq 'length 2 (length '(nil nil)))
(aeq 'assoc nil (assoc 'b nil))
(aeq 'assoc nil (assoc 'b '(nil nil)))
(aeq 'assoc '(b . 12) (assoc 'b '((a . 10) (b . 12))))
(aeq 'assoc '(nil . 12) (assoc nil '((a . 10) (nil . 12))))
(aeq 'assoc '(b) (assoc 'b '((a . 10) (b))))
(aeq 'mapc 2 (cadr (mapc + '(1 2 3 4))))
(aeq 'mapc 10 (let ((x 0)) (mapc (lambda (y) (incf x y)) '(1 2 3 4)) x))
(aeq 'mapcar '(1 4 9 16) (mapcar (lambda (x) (* x x)) '(1 2 3 4)))
(aeq 'mapcar '(1 4 9 16) (mapcar * '(1 2 3 4) '(1 2 3 4)))
(aeq 'mapcar '(1 4 9 16 25) (mapcar (lambda (x) (* x x)) '(1 2 3 4 5)))
(aeq 'mapcan '(1 4 2 5 3 6) (mapcan #'list '(1 2 3) '(4 5 6)))
(aeq 'mapcan '(1 3 2 4) (mapcan list '(1 2) '(3 4)))
(aeq 'mapcan '(1 5 9 2 6 10 3 7 11) (mapcan list '(1 2 3 4) '(5 6 7 8) '(9 10 11)))
(aeq 'mapcan '(1 2 3 . 4) (mapcan (lambda (x) x) '((1) (2) (3 . 4))))
(aeq 'mapcan '(2 3 . 4) (mapcan (lambda (x) x) '(nil (2) (3 . 4))))

#| let/let*/lambda |#

(aeq 'let 7 (let ((x 7)) (let ((x 6) (y x)) y)))
(aeq 'let* 6 (let* ((x 7)) (let* ((x 6) (y x)) y)))
(aeq 'let t (let ((x t) (y nil) (w) z) (and x (null y) (null w) (null z))))
(aeq 'let* t (let* ((x t) (y nil) (w) z) (and x (null y) (null w) (null z))))
(aeq 'lambda 2 ((lambda (x y) (setq y x) y) 2 3))
(aeq 'lambda 9 ((lambda (&rest x) (apply + x)) 2 3 4))
(aeq 'lambda 8 ((lambda (x &optional (y 4)) (* x y)) 2))
(aeq 'lambda 6 ((lambda (x &optional (y 4)) (* x y)) 2 3))
(aeq 'lambda 6 ((lambda (x &optional y) (* x y)) 2 3))
(aeq 'lambda 123 ((lambda (list) list) 123))

#| loops and control |#

(aeq 'progn 8 (let ((x 6)) (progn (incf x) (incf x))))
(aeq 'dotimes 21 (let ((x 6)) (dotimes (y 6 x) (setq x (+ x y)))))
(aeq 'dotimes 6 (let ((x 6)) (dotimes (y 6 y) (setq x (+ x y)))))
(aeq 'dotimes 0 (let ((x 6)) (dotimes (y 0 y) (setq x (+ x y)))))
(aeq 'dolist 6 (let ((x 0)) (dolist (y '(1 2 3) x) (setq x (+ x y)))))
(aeq 'dolist nil (let ((x 0)) (dolist (y '(1 2 3)) (setq x (+ x y)))))
(aeq 'dolist nil (let ((x 0)) (dolist (y '(1 2 3) y) (setq x (+ x y)))))
(aeq 'loop 6 (let ((x 0)) (loop (when (= x 6) (return x)) (incf x))))
(aeq 'loop 6 (let ((x 0)) (loop (unless (< x 6) (return x)) (incf x))))

#| conditions |#

(aeq 'if 3 (let ((a 2)) (if (= a 2) 3 4)))
(aeq 'if 4 (let ((a 2)) (if (= a 3) 3 4)))
(aeq 'if 4 (let ((a 3)) (if (= a 3) 4)))
(aeq 'if nil (let ((a 4)) (if (= a 3) 4)))
(aeq 'when 4 (let ((a 3)) (when (= a 3) 4)))
(aeq 'when nil (let ((a 2)) (when (= a 3) 4)))
(aeq 'unless nil (let ((a 3)) (unless (= a 3) 4)))
(aeq 'unless 4 (let ((a 2)) (unless (= a 3) 4)))
(aeq 'cond 8 (let ((a 2)) (cond ((= a 3) 7) ((= a 2) 8) (t 9))))
(aeq 'cond 9 (let ((a 1)) (cond ((= a 3) 7) ((= a 2) 8) (9))))
(aeq 'cond nil (let ((a 1)) (cond ((= a 3) 7) ((= a 2) 8))))
(aeq 'cond 12 (car (cond ((evenp 3) (list (* 2 3))) ((list (* 3 4))))))
(aeq 'case 222 (let ((j 1)) (case j ((0 1) 111 222) ((t) 333) (t 444))))
(aeq 'case 333 (let ((j t)) (case j ((0 1) 111 222) ((t) 333) (t 444))))
(aeq 'case 444 (let ((j 2)) (case j ((0 1) 111 222) ((t) 333) (t 444))))

#| eval/funcall/apply |#

(aeq 'funcall 10 (funcall + 1 2 3 4))
(aeq 'funcall 'a (funcall car '(a b c d)))
(aeq 'funcall 3 (let ((x 0)) (funcall (lambda (y) (incf x y)) 3) x))
(aeq 'apply 10 (apply + '(1 2 3 4)))
(aeq 'apply 13 (apply + 1 2 '(1 2 3 4)))
(aeq 'eval 10 (eval (list + 1 2 3 4)))
(aeq 'eval nil (eval nil))
(aeq 'funcall 999 (let ((x 999)) (funcall (lambda (x) x) x)))
(aeq 'funcall 4 (let ((x2 (lambda (fun) (lambda (x) (funcall fun (funcall fun x)))))) (funcall (x2 '1+) 2)))
(aeq 'funcall 4 (let ((x2 (lambda (fun) (lambda (x) (fun (fun x)))))) ((x2 '1+) 2)))
(aeq 'apply 5 (let* ((my (lambda (x y) (+ x y))) (han '(my))) (apply (first han) '(2 3))))

#| in-place operations |#

(aeq 'incf 6 (let ((x 0)) (+ (incf x) (incf x) (incf x))))
(aeq 'incf 12 (let ((x 0)) (+ (incf x 2) (incf x 2) (incf x 2))))
(aeq 'incf 36 (let ((n 10)) (let* ((f1 (lambda () (incf n) n))) (+ (funcall f1) (funcall f1) (funcall f1)))))
(aeq 'setf 25 (let ((a 3) (b 4)) (setf a (* a 3) b (* b 4)) (+ a b)))
(aeq 'setf 9 (let ((a '(2 3))) (setf (car a) 6) (apply + a)))
(aeq 'setf 12 (let ((a '(2 3))) (setf (cdr a) '(6)) (apply * a)))
(aeq 'setf 220 (let ((a '(2 3 4))) (setf (nth 1 a) 11 (nth 2 a) 10) (apply * a)))

#| recursion |#

(aeq 'lambda 55 (let ((fib (lambda (n) (if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2))))))) (fib 10)))
(aeq 'lambda 5040 (let ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 7)))
(aeq 'lambda 10 (let ((a 0)) (let ((f (lambda (n) (incf a n) (when (> n 0) (f (1- n)))))) (f 4)) a))

#| streams |#

(aeq 'stream "<string-stream 0>" (with-output-to-string (s) (princ s s)))
(aeq 'stream "12 23 34" (with-output-to-string (st) (format st "~a ~a ~a" 12 23 34)))

#| printing |#

(aeq 'princ "hello" (princ-to-string "hello"))
(aeq 'princ "hello \"David\"" (princ-to-string "hello \"David\""))
(aeq 'prin1 "\"hello\"" (prin1-to-string "hello"))
(aeq 'prin1 "\"hello \\\"David\\\"\"" (prin1-to-string "hello \"David\""))

#| format |#

(aeq 'format "hello" (format nil "hello"))
(aeq 'format "Hello23Goodbye" (format nil "Hello~aGoodbye" 23))
(aeq 'format "   17" (format nil "~5x" 23))
(aeq 'format " 10111" (format nil "~6b" 23))
(aeq 'format "   17    23 23   " (format nil "~5x ~5d ~5a" 23 23 23))
(aeq 'format "00017 00023" (format nil "~5,'0x ~5,'0d" 23 23))
(aeq 'format "01-45-07" (format nil "~2,'0d-~2,'0d-~2,'0d" 1 45 7))
(aeq 'format "Hello42" (format nil "Hello~a" 42))
(aeq 'format "[1,2,3]" (format nil "[~{~a~^,~}]" '(1 2 3)))
(aeq 'format "0003.14159" (format nil "~10,'0g" 3.14159))
(aeq 'format "nil  nil" (format nil "~a ~{ ~a ~} ~a" nil nil nil))

#| strings |#

(aeq 'stringp t (stringp "hello"))
(aeq 'stringp nil (stringp 5))
(aeq 'stringp nil (stringp '(a b)))
(aeq 'numberp nil (numberp "hello"))
(aeq 'atom t (atom "hello"))
(aeq 'consp nil (consp "hello"))
(aeq 'eq nil (eq "hello" "hello"))
(aeq 'eq t (let ((a "hello")) (eq a a)))
(aeq 'length 0 (length ""))
(aeq 'length 5 (length "hello"))
(aeq 'subseq "hello" (subseq "hellofromdavid" 0 5))
(aeq 'subseq "fromdavid" (subseq "hellofromdavid" 5))
(aeq 'concatenate t (string= (concatenate 'string "A" "B") "AB"))
(aeq 'concatenate 3 (length (concatenate 'string "A" "BC")))
(aeq 'concatenate 0 (length (concatenate 'string)))
(aeq 'concatenate "ABCD" (concatenate 'string "AB" "CD"))
(aeq 'concatenate "ABCDE" (concatenate 'string "AB" "CDE"))
(aeq 'concatenate "ABCDE" (concatenate 'string "ABC" "DE"))
(aeq 'concatenate "ABCDEF" (concatenate 'string "ABC" "DEF"))
(aeq 'string< nil (string< "cat" "cat"))
(aeq 'string< t (string< "cat" "cat "))
(aeq 'string< t (string< "fish" "fish "))
(aeq 'string> nil (string> "cat" "cat"))
(aeq 'string> t (string> "cat " "cat"))

#| characters |#

(aeq 'char-code 97 (char-code #\a))
(aeq 'char-code 13 (char-code #\return))
(aeq 'char-code 255 (char-code #\255))
(aeq 'code-char #\return (code-char 13))
(aeq 'code-char #\a (code-char 97))
(aeq 'code-char #\255 (code-char 255))
(aeq 'eq t (eq #\b #\b))
(aeq 'eq nil (eq #\b #\B))
(aeq 'numberp nil (numberp #\b))
(aeq 'characterp t (characterp #\b))
(aeq 'char #\o (char "hello" 4))
(aeq 'char #\h (char "hello" 0))
(aeq 'char "A" (princ-to-string (code-char 65)))
(aeq 'char "[#\\Bell]" (format nil "[~s]" (code-char 7)))
(aeq 'char "[#\\Return]" (format nil "[~s]" #\return))
(aeq 'char "[#\\127]" (format nil "[~s]" #\127))
(aeq 'char "[#\\255]" (format nil "[~s]" #\255))

#| read-from-string |#

(aeq 'read-from-string 123 (read-from-string "123"))
(aeq 'read-from-string 144 (eval (read-from-string "((lambda (x) (* x x)) 12)")))
(aeq 'read-from-string t (eval (read-from-string "(eq (+ 2 3) 5)")))
(aeq 'read-from-string nil (read-from-string "()"))

#| closures |#

(aeq 'closure 'lex (let ((lex nil)) (funcall (let ((lex t)) (lambda () (if lex 'lex 'dyn))))))
(aeq 'closure 103 (let* ((c 100) (two (lambda (d) (+ c d))) (one (lambda (c) (funcall two 3)))) (funcall one 1)))
(aeq 'closure 4 (let ((x 0)) (funcall (lambda (y) (incf x y)) 4) x))
(aeq 'closure 0 (let ((x 0)) (funcall (let ((x 7)) (lambda (y) (setq x (+ x y) ))) 4) x))
(aeq 'closure '(8 10 13 17) (let ((x 0) (clo (lambda () (let ((x 7)) (lambda (y) (incf x y)))))) (mapcar (funcall clo) '(1 2 3 4))))
(aeq 'closure 3 (let ((y 0) (test (lambda (x) (+ x 1)))) (dotimes (x 3 y) (progn (test (+ x 2))) (incf y x))))

#| arrays |#

(aeq 'array '(0 0) (array-dimensions #2a()))
(aeq 'array '(1 0) (array-dimensions #2a(())))
(aeq 'array '(2 0) (array-dimensions #2a(() ())))
(aeq 'array '(0) (array-dimensions (make-array '(0))))
(aeq 'array '(0) (array-dimensions (make-array 0)))
(aeq 'array 1 (let ((a (make-array 3 :initial-element 0))) (incf (aref a (+ 1 1))) (aref a 2)))
(aeq 'array 1 (let ((a (make-array '(3) :initial-element 0))) (incf (aref a (+ 1 1))) (aref a 2)))
(aeq 'array 1 (let ((a (make-array '(2 3) :initial-element 0))) (incf (aref a 1 (+ 1 1))) (aref a 1 2)))
(aeq 'array 1 (let ((a (make-array '(2 3 2 2) :initial-element 0))) (incf (aref a 1 (+ 1 1) 1 1)) (aref a 1 2 1 1)))
(aeq 'array 10 (length (make-array 10 :initial-element 1)))

#| bit arrays |#

(aeq 'array '(0) (array-dimensions (make-array '(0) :element-type 'bit)))
(aeq 'array '(1 1) (array-dimensions (make-array '(1 1) :element-type 'bit)))
(aeq 'array 10 (length (make-array '(10) :element-type 'bit)))
(aeq 'array 10 (length (make-array 10 :element-type 'bit)))
(aeq 'array 1 (let ((a (make-array 3 :element-type 'bit))) (incf (aref a (+ 1 1))) (aref a 2)))
(aeq 'array 1 (let ((a (make-array 3 :initial-element 0 :element-type 'bit))) (incf (aref a (+ 1 1))) (aref a 2)))
(aeq 'array 0 (let ((a (make-array 10 :element-type 'bit :initial-element 1))) (decf (aref a 4)) (aref a 4)))

"#)


(defun run-tests (&optional usb)
  (let ((name (cond
               ((numberp usb) (format nil "/dev/cu.usbmodem~a" usb))
               ((eq usb :esp) "/dev/cu.SLAB_USBtoUART")
               ((eq usb :ftdi) "/dev/cu.usbserial-A104OVGT")
               ;((eq usb :maix) "/dev/cu.usbserial-495223D74D0")
               ((eq usb :maix) "/dev/cu.usbserial-xel_sipeed0")
               ((eq usb :dock) "/dev/cu.wchusbserial1410")
               ((eq usb :teensy) "/dev/cu.usbmodem7705521")
               (t usb)))
        (speed 0.5))
  (flet ((serial-write-exp (string stream)
           (write-string string stream)
           (write-char #\newline stream))
         ;;
         (echo (s)
           (sleep speed)
           (loop
            (let ((c (read-char-no-hang s)))
              (unless c (return))
              (unless (eq c #\return) (write-char c))))
           (format t "~%"))
         ;;
         (read-serial (s)
           (sleep speed)
           (let ((string (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
             (loop
              (let ((c (read-char-no-hang s)))
                (unless c (return string))
                (vector-push-extend c string))))))
    ;;
    (with-open-stream (s (make-instance 'serial-stream :name name))
      (echo s)
      (echo s)
      (serial-write-exp "(defvar ers 0)" s)
      (echo s)
      (serial-write-exp 
       "(defun teq (a b)
         (cond
          ((and (stringp a) (stringp b)) (string= a b))
          ((and (atom a) (atom b)) (eq a b))
          ((null a) nil)
          ((null b) nil)
          (t (and 
              (teq (car a) (car b)) 
              (teq (cdr a) (cdr b))))))"
       s)
      (echo s)
      (serial-write-exp 
       "(defun aeq (tst x y)
          (unless (teq x y)
            (incf ers)
              (princ tst) (princ '=) (princ x) (princ '/) y))"
       s)
      (echo s)
      ;;
      ;; tests
      ;;
      (with-input-from-string (str *tests*)
       (loop
        (let ((line (read-line str nil nil)))
          (unless line (return))
          (serial-write-exp line s)
          (let ((output (read-serial s)))
            (let* ((m1 (position #\return output))
                   (m2 (when m1 (position #\return output :start (+ 2 m1)))))
              (cond
               ((null m2) (format t "~a~%" output))
               ((string= (subseq output (+ 2 m1) m2) "nil") nil)
               (t (format t "*** ~a: ~a~%" (subseq output (+ 2 m1) m2) (subseq output 0 m1)))))))))))))
