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

#"#| Symbols |#

(aeq 'let 123 (let ((cat 123)) cat))
(aeq 'let 79 (let ((ca% 79)) ca%))
(aeq 'let 83 (let ((1- 83)) 1-))
(aeq 'let 13 (let ((12a 13)) 12a))
(aeq 'let 17 (let ((-1- 17)) -1-))
(aeq 'let 66 (let ((abcdef 66)) abcdef))
(aeq 'let 77 (let ((abcdefg 77)) abcdefg))
(aeq 'let 88 (let ((abcdefgh 88)) abcdefgh))
(aeq 'let 99 (let ((abcdefghi 99)) abcdefghi))
(aeq 'let 1010 (let ((abcdefghij 1010)) abcdefghij))
(aeq 'let "ab9" (princ-to-string 'ab9))
(aeq 'let t (eq 'me 'me))
(aeq 'let t (eq 'fishcake 'fishcake))
(aeq 'let nil (eq 'fishcak 'fishca))

#| Arithmetic |#

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
(aeq '< t (< 1 2 3 4))
(aeq '< nil (< 1 2 2 4))
(aeq '< t (<= 1 2 2 4))
(aeq '< nil (<= 1 3 2 4))
(aeq '< t (> 4 3 2 1))
(aeq '< nil (> 4 2 2 1))
(aeq '< t (>= 4 2 2 1))
(aeq '< nil (>= 4 2 3 1))
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
(aeq 'assoc '("three" . 3) (assoc "three" '(("one" . 1) ("two" . 2) ("three" . 3)) :test string=))
(aeq 'member '(3 4) (member 3 '(1 2 3 4)))
(aeq 'member nil (member 5 '(1 2 3 4)))
(aeq 'member '(3 4) (member 3 '(1 2 3 4) :test eq))
(aeq 'member '("three" "four") (member "three" '("one" "two" "three" "four") :test string=))
(aeq 'member '("two" "three" "four") (member "three" '("one" "two" "three" "four") :test string<))

#| map operations |#

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
(aeq 'maplist '(((1 2 3) 6 7 8) ((2 3) 7 8) ((3) 8)) (maplist #'cons '(1 2 3) '(6 7 8)))
(aeq 'maplist '(1 2 3) (mapl #'cons '(1 2 3) '(6 7 8)))
(aeq 'mapcan '(3 7 11) (mapcon (lambda (x) (when (eq (first x) (second x)) (list (car x)))) '(1 2 3 3 5 7 7 8 9 11 11)))

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
(aeq 'return 'a (let ((a 7)) (loop (progn (return 'a)))))
(aeq 'return nil (loop (return)))
(aeq 'return 'a (let ((a 7)) (loop (progn (return 'a) nil))))
(aeq 'do 2 (do* ((x 1 (1+ x)) (y 0 (1+ x))) ((= 3 y) x)))
(aeq 'do 3 (do ((x 1 (1+ x)) (y 0 (1+ x))) ((= 3 y) x)))
(aeq 'do 720 (do* ((n 6) (f 1 (* j f)) (j n (- j 1))) ((= j 0) f)))
(aeq 'do 720 (let ((n 6)) (do ((f 1 (* j f)) (j n (- j 1))  ) ((= j 0) f))))
(aeq 'do 10 (do (a (b 1 (1+ b))) ((> b 10) a) (setq a b)))

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

(aeq 'incf 5 (let ((x 0)) (+ (incf x) (incf x 2) (incf x -2))))
(aeq 'decf -5 (let ((x 0)) (+ (decf x) (decf x 2) (decf x -2))))
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

#| features |#

(aeq 'features t (not (not (member :floating-point *features*))))
(aeq 'features t (not (not (member :arrays *features*))))

#| printing |#

(aeq 'princ "hello" (princ-to-string "hello"))
(aeq 'princ "hello \"David\"" (princ-to-string "hello \"David\""))
(aeq 'prin1 "\"hello\"" (prin1-to-string "hello"))
(aeq 'prin1 "\"hello \\\"David\\\"\"" (prin1-to-string "hello \"David\""))

#| prettyprinting |#

(aeq 'princ "hello" (princ-to-string "hello"))
(aeq 'pprint 10996 (let ((n 0) (st (with-output-to-string (str) (pprint aeq str)))) (dotimes (i (length st) n) (incf n (char-code (char st i))))))

#| documentation |#

(aeq 'apropos '(progn apropos apropos-list unwind-protect) (apropos-list 'pro))
(aeq 'apropos '(progn apropos apropos-list unwind-protect) (apropos-list "pro"))
(aeq 'documentation 7397 (let ((n 0)) (let ((st (documentation '?))) (dotimes (i (length st) n) (incf n (char-code (char st i)))))))

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
(aeq 'concatenate t (string= (concatenate 'string "A" "B") "AB"))
(aeq 'concatenate 3 (length (concatenate 'string "A" "BC")))
(aeq 'concatenate 0 (length (concatenate 'string)))
(aeq 'concatenate "ABCD" (concatenate 'string "AB" "CD"))
(aeq 'concatenate "ABCDE" (concatenate 'string "AB" "CDE"))
(aeq 'concatenate "ABCDE" (concatenate 'string "ABC" "DE"))
(aeq 'concatenate "ABCDEF" (concatenate 'string "ABC" "DEF"))
(aeq 'string= nil (string= "cat" "cat "))
(aeq 'string= t (string= "cat" "cat"))
(aeq 'string/= 3 (string/= "cat" "catx"))
(aeq 'string/= nil (string/= "cat" "cat"))
(aeq 'string/= nil (string/= "catt" "catt"))
(aeq 'string< nil (string< "cat" "cat"))
(aeq 'string<= 3 (string<= "cat" "cat"))
(aeq 'string< 3 (string< "cat" "cat "))
(aeq 'string< 4 (string< "fish" "fish "))
(aeq 'string> nil (string> "cat" "cat"))
(aeq 'string>= 3 (string>= "cat" "cat"))
(aeq 'string>= 5 (string>= "cattx" "cattx"))
(aeq 'string> 0 (string> "c" "a"))
(aeq 'string> 1 (string> "fc" "fa"))
(aeq 'string> 2 (string> "ffc" "ffa"))
(aeq 'string> 3 (string> "fffc" "fffa"))
(aeq 'string> 4 (string> "ffffc" "ffffa"))
(aeq 'string> 5 (string> "fffffc" "fffffa"))
(aeq 'string> nil (string< "fffffc" "fffffa"))
(aeq 'string "albatross" (string "albatross"))
(aeq 'string "x" (string #\x))
(aeq 'string "cat" (string 'cat))
(aeq 'string "albatross" (string 'albatross))


#| subseq and search |#

(aeq 'subseq "hello" (subseq "hellofromdavid" 0 5))
(aeq 'subseq "fromdavid" (subseq "hellofromdavid" 5))
(aeq 'subseq '(2 3 4) (subseq '(0 1 2 3 4) 2))
(aeq 'subseq '(2) (subseq '(0 1 2 3 4) 2 3))
(aeq 'subseq nil (subseq '() 0))
(aeq 'search 4 (search "cat" "the cat sat on the mat"))
(aeq 'search 19 (search "mat" "the cat sat on the mat"))
(aeq 'search nil (search "hat" "the cat sat on the mat"))
(aeq 'search 1 (search '(1 2) '( 0 1 2 3 4)))
(aeq 'search nil (search '(2 1 2 3 4 5) '(2 1 2 3 4)))

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
(aeq 'array 1 (let ((a (make-array 40 :element-type 'bit :initial-element 0))) (incf (aref a 39)) (aref a 39)))
(aeq 'array 0 (let ((a (make-array 40 :element-type 'bit :initial-element 0))) (incf (aref a 39)) (decf (aref a 39)) (aref a 39)))

#| repl |#

(aeq 'repl 23 (read-from-string "23(2)"))
(aeq 'repl nil (read-from-string "()23"))
(aeq 'repl 23 (read-from-string "23\"Hi\""))
(aeq 'repl "Hi" (read-from-string "\"Hi\"23"))
(aeq 'repl #\1 (read-from-string " #\\1\"Hi\""))
(aeq 'repl "Hi" (read-from-string (format nil "\"Hi\"~a~a"  #\# "*0101")))

#| equal |#

(aeq 'equal t (equal '(1 2 3) '(1 2 3)))
(aeq 'equal t (equal '(1 2 (4) 3) '(1 2 (4) 3)))
(aeq 'equal nil (equal '(1 2 (4) 3) '(1 2 (4 nil) 3)))
(aeq 'equal t (equal "cat" "cat"))
(aeq 'equal nil (equal "cat" "Cat"))
(aeq 'equal t (equal 'cat 'Cat))
(aeq 'equal t (equal 2 (+ 1 1)))
(aeq 'equal t (equal '("cat" "dog") '("cat" "dog")))
(aeq 'equal nil (equal '("cat" "dog") '("cat" "dig")))
(aeq 'equal nil (equal '("cat" "dog") '("cat" "Dog")))

#| keywords |#

(aeq 'keywordp t (keywordp :led-builtin))
(aeq 'keywordp nil (keywordp print))
(aeq 'keywordp nil (keywordp nil))
(aeq 'keywordp nil (keywordp 12))
(aeq 'keywordp t (keywordp :fred))
(aeq 'keywordp t (keywordp :initial-element))
(aeq 'keywordp t (keywordp :element-type))

#| errors |#

(aeq 'error 7 (let ((x 7)) (ignore-errors (setq x (/ 1 0))) x))
(aeq 'error 5 (unwind-protect (+ 2 3) 13))

#| Printing floats |#

(aeq 'print t (string= (princ-to-string 101.0) "101.0"))
(aeq 'print t (string= (princ-to-string 1010.0) "1010.0"))
(aeq 'print t (string= (princ-to-string 10100.0) "10100.0"))
(aeq 'print t (string= (princ-to-string 101000.0) "1.01e5"))
(aeq 'print t (string= (princ-to-string 1010000.0) "1.01e6"))
(aeq 'print t (string= (princ-to-string 1.01E7) "1.01e7"))
(aeq 'print t (string= (princ-to-string 1.01E8) "1.01e8"))
(aeq 'print t (string= (princ-to-string 7.0) "7.0"))
(aeq 'print t (string= (princ-to-string 70.0) "70.0"))
(aeq 'print t (string= (princ-to-string 700.0) "700.0"))
(aeq 'print t (string= (princ-to-string 7000.0) "7000.0"))
(aeq 'print t (string= (princ-to-string 70000.0) "70000.0"))
(aeq 'print t (string= (princ-to-string 700000.0) "7.0e5"))
(aeq 'print t (string= (princ-to-string 0.7) "0.7"))
(aeq 'print t (string= (princ-to-string 0.07) "0.07"))
(aeq 'print t (string= (princ-to-string 0.007) "0.007"))
(aeq 'print t (string= (princ-to-string 7.0E-4) "7.0e-4"))
(aeq 'print t (string= (princ-to-string 7.0E-5) "7.0e-5"))
(aeq 'print t (string= (princ-to-string 7.0E-6) "7.0e-6"))
(aeq 'print t (string= (princ-to-string 0.9) "0.9"))
(aeq 'print t (string= (princ-to-string 0.99) "0.99"))
(aeq 'print t (string= (princ-to-string 0.999) "0.999"))
(aeq 'print t (string= (princ-to-string 0.9999) "0.9999"))
(aeq 'print t (string= (princ-to-string 0.99999) "0.99999"))
(aeq 'print t (string= (princ-to-string 0.999999) "0.999999"))
(aeq 'print t (string= (princ-to-string 0.9999999) "1.0"))
(aeq 'print t (string= (princ-to-string 1.0) "1.0"))
(aeq 'print t (string= (princ-to-string 10.0) "10.0"))
(aeq 'print t (string= (princ-to-string 100.0) "100.0"))
(aeq 'print t (string= (princ-to-string 1000.0) "1000.0"))
(aeq 'print t (string= (princ-to-string 10000.0) "10000.0"))
(aeq 'print t (string= (princ-to-string 100000.0) "1.0e5"))
(aeq 'print t (string= (princ-to-string 9.0) "9.0"))
(aeq 'print t (string= (princ-to-string 90.0) "90.0"))
(aeq 'print t (string= (princ-to-string 900.0) "900.0"))
(aeq 'print t (string= (princ-to-string 9000.0) "9000.0"))
(aeq 'print t (string= (princ-to-string 90000.0) "90000.0"))
(aeq 'print t (string= (princ-to-string 900000.0) "9.0e5"))
(aeq 'print t (string= (princ-to-string -9.0) "-9.0"))
(aeq 'print t (string= (princ-to-string -90.0) "-90.0"))
(aeq 'print t (string= (princ-to-string -900.0) "-900.0"))
(aeq 'print t (string= (princ-to-string -9000.0) "-9000.0"))
(aeq 'print t (string= (princ-to-string -90000.0) "-90000.0"))
(aeq 'print t (string= (princ-to-string -900000.0) "-9.0e5"))
(aeq 'print t (string= (princ-to-string 1.0) "1.0"))
(aeq 'print t (string= (princ-to-string 1.01) "1.01"))
(aeq 'print t (string= (princ-to-string 1.001) "1.001"))
(aeq 'print t (string= (princ-to-string 1.0001) "1.0001"))
(aeq 'print t (string= (princ-to-string 1.00001) "1.00001"))
(aeq 'print t (string= (princ-to-string 1.000001) "1.0"))
(aeq 'print t (string= (princ-to-string 0.0012345678) "0.00123457"))
(aeq 'print t (string= (princ-to-string 1.2345678E-4) "1.23457e-4"))
(aeq 'print t (string= (princ-to-string 1234567.9) "1.23457e6"))
(aeq 'print t (string= (princ-to-string 1.2345679E7) "1.23457e7"))
(aeq 'print t (string= (princ-to-string 1.2E-9) "1.2e-9"))
(aeq 'print t (string= (princ-to-string 9.9E-8) "9.9e-8"))
(aeq 'print t (string= (princ-to-string 9.9999E-5) "9.9999e-5"))
(aeq 'print t (string= (princ-to-string 9.01) "9.01"))
(aeq 'print t (string= (princ-to-string 0.9999999) "1.0"))
(aeq 'print t (string= (princ-to-string 0.8999999) "0.9"))
(aeq 'print t (string= (princ-to-string 0.01) "0.01"))
(aeq 'print t (string= (princ-to-string 1.2345679) "1.23457"))
(aeq 'print t (string= (princ-to-string 12.345679) "12.3457"))
(aeq 'print t (string= (princ-to-string 123.45679) "123.457"))
(aeq 'print t (string= (princ-to-string 1234.5679) "1234.57"))
(aeq 'print t (string= (princ-to-string 12345.679) "12345.7"))
(aeq 'print t (string= (princ-to-string 123456.79) "1.23457e5"))
(aeq 'print t (string= (princ-to-string 1234567.9) "1.23457e6"))
(aeq 'print t (string= (princ-to-string 0.12345679) "0.123457"))
(aeq 'print t (string= (princ-to-string 0.012345679) "0.0123457"))
(aeq 'print t (string= (princ-to-string 0.0012345678) "0.00123457"))
(aeq 'print t (string= (princ-to-string 1.2345679E-4) "1.23457e-4"))

#| Arithmetic |#

(aeq '= t (= (- 4 2 1 1) 0))
(aeq '* 9 (* -3 -3))
(aeq '* 32580 (* 180 181))
(aeq '* 1 (*))
(aeq '*  t (string= "-4.29497e9" (princ-to-string (* 2 -2147483648))))
(aeq '* -2147483648 (* 2 -1073741824))
(aeq '+ 32767 (+ 32765 1 1))
(aeq '+ 0 (+))
(aeq '+ -2 (+ -1 -1))
(aeq '- -4 (- 4))
(aeq '/ 2 (/ 60 10 3))
(aeq '1+ 2.5 (1+ 1.5))
(aeq '1+ 2147483647 (1+ 2147483646))
(aeq '1+ t (string= "2.14748e9" (princ-to-string (1+ 2147483647))))
(aeq '1- 0.5 (1- 1.5))
(aeq '1- -2147483648 (1- -2147483647))
(aeq '1- t (string= "-2.14748e9" (princ-to-string (1- -2147483648))))

#| Arithmetic |#

(aeq '/ 1.75 (/ 3.5 2))
(aeq '/ 1.75 (/ 3.5 2.0))
(aeq '/ 0.0625 (/ 1 16))
(aeq '/ 0.0625 (/ 1.0 16))
(aeq '/ 0.0625 (/ 1 16.0))
(aeq '/ 2 (/ 12 2 3))
(aeq '/ 2.0 (/ 12.0 2 3))
(aeq '/ 2.0 (/ 12 2.0 3))
(aeq '/ 2.0 (/ 12 2 3.0))
(aeq '/ 1 (/ 1))
(aeq '/ t (string= "2.14748e9" (princ-to-string (/ -2147483648 -1))))
(aeq '/ 2147483647 (/ -2147483647 -1))
(aeq '/ 0.5 (/ 2))
(aeq '* 1.0 (* 0.0625 16))
(aeq '* 1.0 (* 0.0625 16.0))

#| Place |#

(aeq 'incf 5.4 (let ((x 0)) (+ (incf x) (incf x 0.2) (incf x 2))))
(aeq 'decf -5.4 (let ((x 0)) (+ (decf x) (decf x 0.2) (decf x 2))))
(aeq 'incf 30.6 (let ((n 10)) (let* ((f1 (lambda () (incf n 0.1) n))) (+ (funcall f1) (funcall f1) (funcall f1)))))
(aeq 'setf "hellx" (let ((s "hello")) (setf (char s 4) #\x) s))

#| Comparisons |#

(aeq '< t (< 1 2 3 4))
(aeq '< nil (< 1 2 3 2))
(aeq '< t (< 1.0 2 3 4))
(aeq '< nil (< 1 2 3 2))
(aeq '< t (< 1.0 1.001 3 4))
(aeq '< nil (< 1.001 1.0 3 4))
(aeq '< t (< 1.001 1.002 1.003 1.004))
(aeq '< t (< 1. 2. 3. 4.))
(aeq '< nil (< 1. 2. 2. 4.))
(aeq '< t (<= 1. 2. 2. 4.))
(aeq '< nil (<= 1. 3. 2. 4.))
(aeq '< t (> 4. 3. 2. 1.))
(aeq '< nil (> 4. 2. 2. 1.))
(aeq '< t (>= 4. 2. 2. 1.))
(aeq '< nil (>= 4. 2. 3. 1.))
(aeq '/= t (= 1. 1. 1. 1.))
(aeq '/= nil (= 1. 1. 2. 1.))
(aeq '/= nil (/= 1. 2. 3. 1.))
(aeq '/= t (/= 1. 2. 3. 4.))

#| Transcendental |#

(aeq 'sin 0.84147096 (sin 1))
(aeq 'sin 0.0 (sin 0))
(aeq 'sin 0.84147096 (sin 1.0))
(aeq 'sin 0.0 (sin 0.0))
(aeq 'cos 0.540302 (cos 1))
(aeq 'cos 0.540302 (cos 1.0))
(aeq 'tan 1.55741 (tan 1))
(aeq 'tan 1.55741 (tan 1.0))
(aeq 'asin 1.5707964 (asin 1))
(aeq 'asin 1.5707964 (asin 1))
(aeq 'asin 0.0 (asin 0))
(aeq 'asin 0.0 (asin 0.0))
(aeq 'acos 0.0 (acos 1))
(aeq 'acos 0.0 (acos 1.0))
(aeq 'acos 1.0471976 (acos 0.5))
(aeq 'atan 0.4636476 (atan 0.5))
(aeq 'atan 0.110657 (atan 1 9))
(aeq 'atan 0.049958397 (atan 1 20))
(aeq 'atan 0.785398 (atan 1 1))
(aeq 'atan 0.785398 (atan .5 .5))x
(aeq 'sinh 1.1752 (sinh 1))
(aeq 'sinh 1.1752 (sinh 1.0))
(aeq 'sinh 0.0 (sinh 0))
(aeq 'sinh 0.0 (sin 0.0))
(aeq 'cosh 1.5430807 (cosh 1))
(aeq 'cosh 1.5430807 (cosh 1.0))
(aeq 'tanh 0.7615942 (tanh 1))
(aeq 'tanh 0.7615942 (tanh 1.0))

#| Rounding |#

(aeq 'truncate 3 (truncate 10 3))
(aeq 'truncate 3 (truncate 3.3333333))
(aeq 'ceiling 4 (ceiling 10 3))
(aeq 'ceiling 4 (ceiling 3.3333333))
(aeq 'round 3 (round 10 3))
(aeq 'round 3 (round 3.3333333))
(aeq 'floor 3 (floor 10 3))
(aeq 'floor 3 (floor 3.3333333))
(aeq 'truncate -3 (truncate -10 3))
(aeq 'truncate -3 (truncate -3.3333333))
(aeq 'ceiling -3 (ceiling -10 3))
(aeq 'ceiling -3 (ceiling -3.3333333))
(aeq 'round -3 (round -10 3))
(aeq 'round -3 (round -3.3333333))
(aeq 'floor -4 (floor -10 3))
(aeq 'floor -4 (floor -3.3333333))
(aeq 'abs 10.0 (abs 10.0))
(aeq 'abs 10.0 (abs -10.0))
(aeq 'abs t (string= "2.14748e9" (princ-to-string (abs -2147483648))))
(aeq 'abs 2147483647 (abs -2147483647))
(aeq 'mod 1.0 (mod 13.0 4))
(aeq 'mod 3.0 (mod -13.0 4))
(aeq 'mod -3.0 (mod 13.0 -4))
(aeq 'mod -1.0 (mod -13.0 -4))
(aeq 'mod -3.0 (mod 13.0 -4))
(aeq 'mod 1.0 (mod -12.5 1.5))
(aeq 'mod 0.5 (mod 12.5 1.5))

#| Log and exp |#

(aeq 'exp 2.7182818 (exp 1))
(aeq 'exp 2.7182818 (exp 1.0))
(aeq 'exp 0.36787945 (exp -1))
(aeq 'exp 0.36787945 (exp -1.0))
(aeq 'exp 0.36787945 (exp -1.0))
(aeq 'log 0.0 (log 1.0))
(aeq 'log 4.0 (log 16 2))
(aeq 'log 4.0 (log 16.0 2))
(aeq 'log 4.0 (log 16 2.0))
(aeq 'log 4.0 (log 16.0 2.0))
(aeq 'log 1.0 (log 2 2))
(aeq 'log 1.0 (log 2.5 2.5))
(aeq 'log 2.3025852 (log 10))
(aeq 'log 2.3025852 (log 10))
(aeq 'expt 1024 (expt 2 10))
(aeq 'expt 1024.0 (expt 2.0 10.0))
(aeq 'expt 1073741824 (expt 2 30))
(aeq 'expt  t (string= "2.14748e9" (princ-to-string (expt 2 31))))
(aeq 'expt  t (string= "4.29497e9" (princ-to-string (expt 2 32))))
(aeq 'expt 1024 (expt -2 10))
(aeq 'expt -2048 (expt -2 11))

#| Tests |#

(aeq 'floatp nil (floatp 1))
(aeq 'floatp nil (floatp nil))
(aeq 'floatp t (floatp 2.3))
(aeq 'integerp t (integerp 1))
(aeq 'integerp nil (integerp nil))
(aeq 'integerp nil (integerp 2.3))

#| error checks |#

(aeq 'dolist nothing (ignore-errors (dolist 12 (print x))))
(aeq 'dolist nothing (ignore-errors (dolist () (print x))))
(aeq 'dolist nothing (ignore-errors (dolist (x) (print x))))
(aeq 'dolist nothing (ignore-errors (dolist (x nil x x) (print x))))
(aeq 'dotimes nothing (ignore-errors (dotimes 12 (print x))))
(aeq 'dotimes nothing (ignore-errors (dotimes () (print x))))
(aeq 'dotimes nothing (ignore-errors (dotimes (x) (print x))))
(aeq 'dotimes nothing (ignore-errors (dotimes (x 1 x x) (print x))))
(aeq 'for-millis nothing (ignore-errors (for-millis 12 (print 12))))
(aeq 'for-millis nothing (ignore-errors (for-millis (12 12) (print 12))))
(aeq 'push nothing (ignore-errors (let ((a #*00000000)) (push 1 (aref a 1)) a)))
(aeq 'setf nothing (ignore-errors (let ((s "hello")) (setf (char s 5) #\x) s)))
(aeq 'setf nothing (ignore-errors (let ((s "hello")) (setf (char s 20) #\x) s)))

#| errors |#

(aeq 'errors 0 ers)

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
        (speed 1))
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
       "(defun aeq (tst x y)
          (unless (or
              (and (floatp x) (floatp y) (< (abs (- x y)) 0.000005))
              (equal x y))
            (incf ers)
              (format t \"~a=~a/~a~%\" tst x y)))"
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
