(in-package :asdf)

(defsystem dawg
  :name "dawg"
  :author "Takeru Ohta"
  :version "0.0.4"
  :description "Direct Acyclic Word Graph"
  
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "byte-stream")
               (:file "double-array_node-allocator")
               (:file "double-array")
               (:file "dawg")))
