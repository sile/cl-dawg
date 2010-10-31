(defpackage dawg.global
  (:use :common-lisp)
  (:export ;; special variable
           *fastest*
           *interface*

           ;; type
           array-index
           positive-fixnum
           octet
           simple-characters
           unicode

           ;; utility function
           fixnumize
           package-alias
           muffle-compiler-note))
(in-package :dawg.global)

(defvar *fastest* '(optimize (speed 3) (safety 0) (debug 0)))
(defvar *interface* '(optimize (speed 3) (safety 2) (debug 1)))

(deftype array-index () `(mod ,array-dimension-limit))
(deftype positive-fixnum () `(integer 0 ,most-positive-fixnum))
(deftype octet () '(unsigned-byte 8))
(deftype simple-characters () '(simple-array character))
(deftype unicode () `(mod ,char-code-limit))

(declaim (inline fixnumize))
(defun fixnumize (n)
  (ldb (byte #.(integer-length most-positive-fixnum) 0) n))

(defmacro package-alias (package &rest alias-list)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (rename-package ,package ,package ',alias-list)))

(defmacro muffle-compiler-note (&body body)
  `(locally
    (declare #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
    ,@body))
