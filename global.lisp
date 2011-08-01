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
           uint8
           uint4
           uint1

           ;; utility function
           fixnumize
           package-alias
           muffle
           a.if
           nlet
           
           ;; symbol for anaphoric macro
           it))
(in-package :dawg.global)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; special variable for optimize declaration
(defvar *fastest* '(optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
(defvar *interface* '(optimize (speed 3) (safety 2) (debug 1) (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;
;;; type definition
(deftype array-index () `(mod ,array-dimension-limit))
(deftype positive-fixnum () `(integer 0 ,most-positive-fixnum))
(deftype octet () '(unsigned-byte 8))
(deftype simple-characters () '(simple-array character))
(deftype unicode () `(mod ,char-code-limit))
(deftype uint8 () '(unsigned-byte 64))
(deftype uint4 () '(unsigned-byte 32))
(deftype uint1 () '(unsigned-byte 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unility function and macro
(declaim (inline fixnumize write-bigendian-uint4 read-bigendian-uint4))
(defun fixnumize (n)
  (ldb (byte #.(integer-length most-positive-fixnum) 0) n))

(defmacro package-alias (package &rest alias-list)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (rename-package ,package ,package ',alias-list)))

(defmacro muffle (&body body)
  `(locally
    (declare #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
    ,@body))

(defmacro a.if (exp then else)
  `(let ((it ,exp))
     (if it
         ,then
       ,else)))

(defmacro nlet (fn-name letargs &body body)
  `(labels ((,fn-name ,(mapcar #'car letargs)
              ,@body))
     (,fn-name ,@(mapcar #'cadr letargs))))
