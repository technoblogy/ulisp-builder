;; Options: :avr :arm :msp430 :esp :stm32 :badge :zero :riscv

(push :esp *features*)
 
;***************************************

;(push :interrupts *features*)

#+badge
(push :arm *features*)

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


(load "/Users/david/Projects/Builder/builder defsys.lisp")

(map nil #'delete-file (directory "/Users/david/Projects/Builder/fasls/*"))

(compile-system "builder" :load t) 