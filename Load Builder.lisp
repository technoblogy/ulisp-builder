;; Options: :avr :arm :msp430 :esp :stm32 :badge :zero :riscv

(push :avr *features*)

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


(load "/Users/david/Projects/Builder/builder defsys.lisp")

(compile-system "builder" :load t) 