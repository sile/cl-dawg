(defpackage dawg
  (:use :common-lisp :dawg.global)
  (:shadow :common-lisp load)
  (:export build
           load
           member?
           get-id
           each-common-prefix))
(in-package :dawg)

   
