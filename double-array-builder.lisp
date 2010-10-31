(defpackage :dawg.double-array-builder
  (:use :common-lisp :dawg.global)
  (:shadow :common-lisp load)
  (:export build-from-bintrie
           save
           load))
(in-package :dawg.double-array-builder)


           