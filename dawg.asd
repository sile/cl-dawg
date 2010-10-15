(in-package :asdf)

(defsystem dawg
  :name "dawg"
  :author "Takeru Ohta"
  :version "0.0.1"
  :description "Direct Acyclic Word Graph"
  
  :depends-on (:dict)
  
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "byte-stream")
               (:file "dawg")))
