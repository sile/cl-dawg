(in-package :asdf)

(defsystem dawg
  :name "dawg"
  :author "Takeru Ohta"
  :version "0.1.0"
  
  :serial t
  :components ((:file "global")
               (:file "package")
               (:file "octet-stream")
               (:file "bintrie-builder")
               (:file "double-array-node-allocator")
               (:file "double-array-builder")
               (:file "dawg")))
