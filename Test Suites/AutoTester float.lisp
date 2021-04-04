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

#"#| Printing floats |#

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

#| Comparisons |#

(aeq '< t (< 1 2 3 4))
(aeq '< nil (< 1 2 3 2))
(aeq '< t (< 1.0 2 3 4))
(aeq '< nil (< 1 2 3 2))
(aeq '< t (< 1.0 1.001 3 4))
(aeq '< nil (< 1.001 1.0 3 4))
(aeq '< t (< 1.001 1.002 1.003 1.004))

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
       "(defun aeq (tst x y)
         (unless 
             (or
              (and (floatp x) (floatp y) (< (abs (- x y)) 0.000005))
              (eq x y))
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


