;; Options: :avr :avr-nano :arm :msp430 :esp :stm32 :badge :zero :riscv

(push :avr-nano *features*)

(defparameter *release* "4.6")
(defparameter *date* "13th June 2024")
 
;***************************************

#+badge
(push :avr *features*)

#+(or arm esp stm32 riscv)
(push :float *features*)

#+(or arm esp stm32 riscv avr)
(push :arrays *features*)

#+(or arm esp)
(push :wifi *features*)

#+(or riscv arm esp)
(push :gfx *features*)

#+(or arm esp riscv avr)
(push :doc *features*)

#+(or arm esp riscv avr)
(push :errors *features*)

(load "/Users/david/Projects/Builder/builder defsys.lisp")

(map nil #'delete-file (directory "/Users/david/Projects/Builder/fasls/*"))

(compile-system "builder" :load t) 