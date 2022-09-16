;; Options: :avr :arm :msp430 :esp :stm32 :badge :zero :riscv

(push :avr *features*)
 
;***************************************

;(push :interrupts *features*)

#+badge
(push :avr *features*)

#+(or arm esp stm32 riscv)
(push :float *features*)

#+(or arm esp stm32 riscv avr)
(push :arrays *features*)

#+(or arm esp)
(push :ethernet *features*)

#+(or riscv arm esp)
(push :gfx *features*)

#+(or riscv arm)
(push :code *features*)

#+(or arm esp riscv avr)
(push :doc *features*)

(load "/Users/david/Projects/Builder/builder defsys.lisp")

(map nil #'delete-file (directory "/Users/david/Projects/Builder/fasls/*"))

(compile-system "builder" :load t) 