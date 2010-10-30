(in-package :dawg)

(declaim (inline fixnumize))

(defun fixnumize (n)
  (ldb (byte +FIXNUM-LENGTH+ 0) n))
