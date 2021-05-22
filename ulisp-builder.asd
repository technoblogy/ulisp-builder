;;;; ulisp-builder.asd

(asdf:defsystem #:ulisp-builder
  :description "Produce uLisp Arduino source code"
  :author "David Johnson-Davies <david@interface.co.uk>"
  :license "MIT"
  :version "3.6"
  :serial t
  :components ((:file "extras")
               (:file "functions")
               (:file "preface")
               (:file "utilities")
               (:file "saveload")
               (:file "prettyprint")
               (:file "assembler")
               (:file "postscript")
               (:file "avr")
               (:file "arm")
               (:file "esp")
               (:file "riscv")
               (:file "badge")
               (:file "build")))
