(defpackage dawg.double-array.buffered-output
  (:use :common-lisp :dawg.global)
  (:export buffered-output
           with-output
           write-uint))
(in-package :dawg.double-array.buffered-output)

(declaim #.*fastest*)

(defconstant +BUFFER_SIZE+ 819200)

(defstruct buffered-output
  (binary-output nil :type file-stream)
  (buffer        #() :type simple-array)
  (offset          0 :type array-index))

(defmacro with-output ((out path &key (byte-width 1)) &body body)
  (declare ((mod 5) byte-width))
  `(with-open-file (,out ,path :element-type #1='(unsigned-byte ,(* 8 byte-width))
                               :direction :output
                               :if-exists :supersede)
     (let ((,out (make-buffered-output 
                  :binary-output ,out
                  :buffer (make-array ,+BUFFER_SIZE+ :element-type #1#
                                                     :initial-element 0))))
       (unwind-protect
           (locally ,@body)
         (flush ,out :final t)))))

(defun write-uint (uint out &key (position 0))
  (declare (buffered-output out)
           (positive-fixnum position))
  (with-slots (binary-output buffer offset) out
    (cond ((< position offset)
           (file-position binary-output position)
           (write-byte uint binary-output))
          ((< position (+ offset +BUFFER_SIZE+))
           (muffle-compiler-note
            (setf (aref buffer (- position offset)) uint)))
          (t
           (flush out)
           (incf offset +BUFFER_SIZE+)
           (fill buffer 0)
           (write-uint uint out :position position)))))

(defun flush (out &key final)
  (declare (buffered-output out))
  (with-slots (binary-output buffer offset) out
    (file-position binary-output offset)
    (if (null final)
        (write-sequence buffer binary-output)
      (let ((end (muffle-compiler-note
                  (or (position 0 buffer :from-end t)
                      +BUFFER_SIZE+))))
        (write-sequence buffer binary-output :end end)
        (loop REPEAT #x100 DO (write-byte 0 binary-output))))))
