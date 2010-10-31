(defpackage dawg.octet-stream
  (:use :common-lisp :dawg.global)
  (:shadow :common-lisp read peek)
  (:export make
           read
           peek
           eos?
           eat))
(in-package :dawg.octet-stream)

(declaim #.*fastest*
         (inline make-octet-stream make eos? octet-length peek read eat))

(defstruct octet-stream
  (src      "" :type simple-characters)
  (pos       0 :type array-index)
  (end       0 :type array-index)
  (code      0 :type unicode)
  (octet-pos 0 :type (mod 5))
  (octet-len 0 :type (mod 5)))
             

(defun octet-length (code)
  (declare (unicode code))
  (cond ((< code #x80)    1)
        ((< code #x800)   2)
        ((< code #x10000) 3)
        (t                4)))

(defun make (string &key (start 0) (end (length string)))
  (declare (simple-characters string)
           (array-index start end))
  (let* ((code (if (= start (length string)) 
                  0
                (char-code (char string start))))
         (len (octet-length code)))
    (make-octet-stream :src string :pos start :end end
                       :code code :octet-pos len :octet-len len)))

(defun eos? (in)
  (with-slots (src pos octet-pos) (the octet-stream in)
    (= pos (length src))))

(defun peek (in)
  (with-slots (src pos code octet-pos octet-len) (the octet-stream in)
    (if (= octet-pos octet-len)
        (case octet-len
            (1 code)
            (2 (+ #b11000000 (ldb (byte 5  6) code)))
            (3 (+ #b11100000 (ldb (byte 4 12) code)))
            (4 (+ #b11110000 (ldb (byte 3 18) code))))
      (+ #b10000000 (ldb (byte 6 (* (the (mod 4) (1- octet-pos)) 6)) code)))))

(defun eat (in)
  (with-slots (src pos code octet-pos octet-len) (the octet-stream in)
    (decf octet-pos)
    (when (zerop octet-pos)
      (incf pos)
      (unless (eos? in)
        (setf code (char-code (char src pos))
              octet-len (octet-length code)
              octet-pos octet-len))))
  in)

(defun read (in)
  (prog1 (peek in)
    (eat in)))
