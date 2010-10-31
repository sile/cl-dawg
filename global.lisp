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
           uint4
           uint1

           ;; utility function
           fixnumize
           package-alias
           muffle-compiler-note
           a.if
           write-bigendian-uint4
           read-bigendian-uint4

           ;; symbol for anaphoric macro
           it))
(in-package :dawg.global)

(defvar *fastest* '(optimize (speed 3) (safety 0) (debug 0)))
(defvar *interface* '(optimize (speed 3) (safety 2) (debug 1)))

(deftype array-index () `(mod ,array-dimension-limit))
(deftype positive-fixnum () `(integer 0 ,most-positive-fixnum))
(deftype octet () '(unsigned-byte 8))
(deftype simple-characters () '(simple-array character))
(deftype unicode () `(mod ,char-code-limit))
(deftype uint4 () '(unsigned-byte 32))
(deftype uint1 () '(unsigned-byte 8))

(declaim (inline fixnumize write-bigendian-uint read-bigendian-uint))
(defun fixnumize (n)
  (ldb (byte #.(integer-length most-positive-fixnum) 0) n))

(defmacro package-alias (package &rest alias-list)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (rename-package ,package ,package ',alias-list)))

(defmacro muffle-compiler-note (&body body)
  `(locally
    (declare #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
    ,@body))

(defmacro a.if (exp then else)
  `(let ((it ,exp))
     (if it
         ,then
       ,else)))

(defun write-bigendian-uint4 (uint out)
  (loop FOR offset FROM 24 DOWNTO 0 BY 8
        DO (write-byte (ldb (byte 8 offset) uint) out)))

(defun read-bigendian-uint4 (in)
  (loop FOR offset FROM 24 DOWNTO 0 BY 8
        SUM (ash (read-byte in) offset)))

