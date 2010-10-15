(defpackage dawg.byte-stream
  (:use :common-lisp)
  (:shadow :common-lisp read peek)
  (:export make
           read
           peek
           eat
           eos?))
(in-package :dawg.byte-stream)

(declaim (inline make read peek eat eos?))

(defstruct (byte-stream (:constructor make (bytes &key (start 0) (end (length bytes))
                                                  &aux (cur start))))
  (bytes #() :type dawg::simple-octets)
  (cur     0 :type dawg::array-index)
  (end     0 :type dawg::array-index))

(defun eat (in)
  (declare #.dawg::*fastest*)
  (incf (byte-stream-cur in))
  in)

(defun eos? (in)
  (declare #.dawg::*fastest*)
  (>= (byte-stream-cur in) (byte-stream-end in)))

(defun peek (in)
  (declare #.dawg::*fastest*)
  (if (eos? in)
      0
    (with-slots (bytes cur) in
      (aref bytes cur))))

(defun read (in)
  (declare #.dawg::*fastest*)
  (prog1 (peek in)
    (eat in)))
           