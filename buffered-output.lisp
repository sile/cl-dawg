(defpackage :dawg.buffered-output
  (:use :common-lisp)
  (:shadow :common-lisp write-byte)
  (:export buffered-output
           with-buffered-output
           write-byte))
(in-package :dawg.buffered-output)

(defconstant +BUFFER_SIZE+ 81920)

(defstruct buffered-output
  (binary-output nil :type stream)
  (buffer        #() :type simple-array)
  (default-value   0 :type fixnum)
  (offset          0 :type fixnum))
    

(defmacro with-buffered-output ((out path &key (byte-width 1) (initial-element 0)) &body body)
  `(with-open-file (,out ,path :element-type #1='(unsigned-byte ,(* 8 byte-width))
                               :direction :output
                               :if-exists :supersede)
     (let ((,out (make-buffered-output 
                  :binary-output ,out
                  :buffer (make-array ,+BUFFER_SIZE+ :element-type #1#
                                                     :initial-element ,initial-element))))
       (unwind-protect
           (locally ,@body)
         (flush ,out)))))
         
(defun write-byte (byte out &key (position 0))
  (declare (buffered-output out)
           #.dawg::*fastest*
           (fixnum position))
  (with-slots (binary-output buffer offset default-value) out
    (cond ((< position offset)
           (file-position binary-output position)
           (common-lisp:write-byte byte binary-output))
          ((< position (+ offset (length buffer)))
           (setf (aref buffer (- position offset)) byte))
          (t
           (flush out)
           (incf offset (length buffer))
           (fill buffer default-value)
           (write-byte byte out :position position))))
  t)

(defun flush (out)
  (declare (buffered-output out))
  (with-slots (binary-output buffer offset) out
    (file-position binary-output offset)
    (write-sequence buffer binary-output))
  t)