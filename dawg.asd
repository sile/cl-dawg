(in-package :asdf)

(defsystem dawg
  :name "dawg"
  :author "Takeru Ohta"
  :version "0.1.0"
  :description "Direct Acyclic Word Graph"
  
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "char-stream")
               (:file "dawg")))
