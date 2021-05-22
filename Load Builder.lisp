;; Options: :avr :arm :msp430 :esp :stm32 :badge :zero :riscv

#+lispworks
(push :esp *features*)
 
;***************************************

;(push :interrupts *features*)

#+badge
(push :avr *features*)

#+(or arm esp stm32 riscv)
(push :float *features*)

#+(or arm esp stm32 riscv)
(push :arrays *features*)

#+(or esp)
(push :ethernet *features*)

#+(or riscv arm esp)
(push :gfx *features*)

#+(or riscv arm)
(push :code *features*)


#+lispworks
(progn
  (load "/Users/david/Projects/Builder/builder defsys.lisp")
  (compile-system "builder" :load t))

#-lispworks
(progn
  (require "asdf")
  (defvar *this-directory* (uiop:pathname-directory-pathname *load-pathname*))
  (pushnew *this-directory* asdf:*central-registry* :test 'equal)
  (asdf:load-system "ulisp-builder"))