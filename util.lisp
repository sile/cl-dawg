(in-package :dawg)

(defconstant +LINE-FEED+ #\Newline)
(defconstant +BUFFER-SIZE-LIMIT+ 102400) 

(declaim (inline string-to-octets each-file-line-bytes-impl fixnumize get-obj-address))

(defmacro package-alias (package &rest alias-list)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (rename-package ,package ,package ',alias-list)))

(defun get-obj-address (obj)
  #+SBCL (sb-kernel:get-lisp-obj-address obj)
  #-SBCL (error "TODO: error message"))

(defun string-to-octets (str)
  #+SBCL (sb-ext:string-to-octets str)
  #-SBCL (error "TODO: error message"))

(defun fixnumize (n)
  (ldb (byte +FIXNUM-LENGTH+ 0) n))

(defmacro write-uint (num-exp byte-width out)
  (let ((int-type `(unsigned-byte ,(* byte-width 8))))
    `(progn
       ,@(loop WITH #2=#:num = num-exp
               FOR #1=#:i FROM 0 BELOW byte-width
           COLLECT
           `(write-byte (ldb (byte 8 ,(* #1# 8)) (the ,int-type ,#2#)) ,out)))))

(defmacro read-uint (byte-width in)
  `(the (unsigned-byte ,(* byte-width 8))
        (+ ,@(loop FOR #1=#:i FROM 0 BELOW byte-width
               COLLECT
               `(ash (read-byte ,in) ,(* #1# 8))))))

(defmacro each-file-line-bytes ((line-bytes start end filepath) &body body)
  `(each-file-line-bytes-impl 
    (lambda (,line-bytes ,start ,end)
      (declare (simple-octets ,line-bytes)
               ((mod #.array-dimension-limit) ,start ,end))
      ,@body)
    ,filepath))

(defun each-file-line-bytes-impl (fn filepath)
  (declare #.*muffle-warning*
           #.*fastest*
           (function fn))
  (with-open-file (in filepath :element-type 'octet)
    (let* ((buffer-size (min (or (file-length in) #1=+BUFFER-SIZE-LIMIT+) #1#))
           (buf (make-array buffer-size :element-type 'octet))
           (read-start 0)
           (lf (char-code +LINE-FEED+))
           (stack '()))
      (loop FOR read-len = (read-sequence buf in :start read-start)
        DO
        (loop WITH start = 0 
              FOR lf-pos =  (position lf buf :start read-start :end read-len)
                       THEN (position lf buf :start start      :end read-len)
              WHILE lf-pos
          DO
          (if (null stack)
              (funcall fn buf start lf-pos)
            (let ((bytes (apply #'concatenate 'octets 
                                (nreverse (cons (subseq buf start lf-pos) stack)))))
              (funcall fn bytes 0 (length bytes))
              (setf stack nil)))
          
          (setf start (1+ lf-pos))

          FINALLY
          (setf read-start 0)
          (if (zerop start) 
              (push (copy-seq buf) stack) 
            (progn (setf read-start (- read-len start))
                   (replace buf buf :end1 read-start :start2 start :end2 read-len))))
        
        (when (< read-len buffer-size)
          (return))))))
