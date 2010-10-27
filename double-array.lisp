(defpackage :dawg.double-array
  (:use :common-lisp)
  (:shadow :common-lisp load)
  (:export build-from-trie
           member?
           save
           load))

(in-package :dawg.double-array)

(dawg::package-alias :dawg.byte-stream :byte-stream)
(dawg::package-alias :dawg.double-array.node-allocator :node-allocator)
(dawg::package-alias :dawg.buffered-output :buffered-output)

(declaim (inline resize set-node))

(deftype uint4 () '(unsigned-byte 32))
(deftype uint1 () '(unsigned-byte 8))

(defstruct double-array
  (base nil :type buffered-output:buffered-output)
  (chck nil :type buffered-output:buffered-output))

(defun save (filepath da)
  (declare (ignorable filepath da))
  #+IGNORE
  (with-slots (base chck) da
    (close base)
    (close chck))
  t)

(defun set-node (da node-idx base-idx arc &aux (next-idx (+ base-idx arc)))
  (declare (fixnum base-idx next-idx)
           ((mod #x100) arc))
  (with-slots (base chck) (the double-array da)
    (buffered-output:write-byte base-idx base :position node-idx)
    (buffered-output:write-byte arc      chck :position next-idx)
#|    
    (file-position base node-idx)
    (write-byte base-idx base)

    (file-position chck next-idx)
    (write-byte arc chck)
|#
    next-idx))

(defun build-from-trie-impl (trie alloca da node-idx)
  (declare #.dawg::*fastest*)
  (when (zerop (mod (the fixnum node-idx) 5000))
    (print node-idx))

  (let ((children (dawg::collect-children trie)))
    (when children
      (let ((base-idx (node-allocator:allocate
                       alloca 
                       (mapcar #'dawg::node-label children))))
        (dolist (child children)
          (build-from-trie-impl 
           child alloca da
           (set-node da node-idx base-idx (dawg::node-label child))))))))

(defun build-from-trie (trie)
  (buffered-output:with-buffered-output (out1 "/tmp/base" :byte-width 4)
    (buffered-output:with-buffered-output (out2 "/tmp/chck" :initial-element #xFF)
      (let ((da (make-double-array :base out1 :chck out2)))
        (build-from-trie-impl trie (node-allocator:make) da 0)
        da))))

(defstruct da2
  (base #() :type (simple-array uint4))
  (chck #() :type (simple-array uint1)))

(defun member?-impl (in node base chck)
  (let ((next (+ (aref base node) (byte-stream:peek in))))
    (when (= (aref chck next) (byte-stream:peek in))
      (if (byte-stream:eos? in)
          t
        (member?-impl (byte-stream:eat in) next base chck)))))

(defun member?(key da)
  (with-slots (base chck) (the da2 da)
    (member?-impl (byte-stream:make (dawg::string-to-octets key))
                  0
                  base chck)))

(defun load (filepath)
  (declare (ignorable filepath))
  (with-open-file (in "/tmp/base" :element-type 'uint1)
    (with-open-file (in2 "/tmp/chck" :element-type 'uint1)
    (let* ((base-len (/ (file-length in) 4))
           (chck-len (file-length in2))
           (da (make-da2 :base (make-array base-len :element-type 'uint4)
                         :chck (make-array chck-len :element-type 'uint1))))
      (with-slots (base chck) da
        (loop FOR i FROM 0 BELOW base-len
          DO
          (setf (aref base i) (dawg::read-uint 4 in)))
        (loop FOR i FROM 0 BELOW chck-len
          DO
          (setf (aref chck i) (dawg::read-uint 1 in2))))
      da))))

(dawg::package-alias :dawg.byte-stream)
(dawg::package-alias :dawg.double-array.node-allocator)
(dawg::package-alias :dawg.buffered-output)