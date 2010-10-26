(defpackage dawg
  (:use :common-lisp)
  (:export build-from-file
           save-as-double-array
           member?
           node-count))
(in-package :dawg)

(defvar *fastest*   '(optimize (speed 3) (safety 0) (debug 0)))
(defvar *interface* '(optimize (speed 3) (safety 2) (debug 1)))
(defvar *muffle-warning* #+SBCL '(sb-ext:muffle-conditions sb-ext:compiler-note) 
                         #-SBCL '())

(deftype octet () '(unsigned-byte 8))
(deftype octets () '(vector octet))
(deftype simple-octets () '(simple-array octet))
(deftype array-index () `(mod ,array-dimension-limit))
(deftype positive-fixnum () `(integer 0 ,most-positive-fixnum))

(defconstant +FIXNUM-LENGTH+ (integer-length most-positive-fixnum))


