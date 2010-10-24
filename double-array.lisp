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

(declaim (inline resize set-node))

(deftype uint4 () '(unsigned-byte 32))
(deftype uint1 () '(unsigned-byte 8))

(defun resize (array index &optional (default 0))
  (loop WHILE (<= (length array) index) DO
    (setf array (adjust-array array (the fixnum (max 1 (* (length array) 2)))
                              :initial-element default)))
  array)

(defstruct double-array 
  (base #() :type (simple-array uint4))
  (chck #() :type (simple-array uint1)))

(defun save (filepath da)
  (with-open-file (out filepath :direction :output
                                :if-exists :supersede
                                :element-type 'uint1)
    (with-slots (base chck) (the double-array da)
      (let* ((last (position-if (lambda (x) (/= x #xFF)) chck :from-end t))
             (size (+ last #x100)))
        (setf base (adjust-array base size :initial-element 0)
              chck (adjust-array chck size :initial-element #xFF))
        
        (print (list (count #xFF chck) (length chck)))
        (dawg::write-uint (length base) 4 out)
        (dawg::write-uint (length chck) 4 out)

        (loop FOR b ACROSS base
              DO (dawg::write-uint b 4 out))
        (loop FOR c ACROSS chck
              DO (dawg::write-uint c 1 out)))))
  t)

(defun load (filepath)
  (with-open-file (in filepath :element-type 'uint1)
    (let* ((base-len (dawg::read-uint 4 in))
           (chck-len (dawg::read-uint 4 in))
           (da (make-double-array :base (make-array base-len :element-type 'uint4)
                                  :chck (make-array chck-len :element-type 'uint1))))
      (with-slots (base chck) da
        (loop FOR i FROM 0 BELOW base-len
          DO
          (setf (aref base i) (dawg::read-uint 4 in)))
        (loop FOR i FROM 0 BELOW chck-len
          DO
          (setf (aref chck i) (dawg::read-uint 1 in))))
      da)))

(defun set-node (da node-idx base-idx arc &aux (next-idx (+ base-idx arc)))
  (declare (fixnum base-idx)
           ((mod #x100) arc))
  (with-slots (base chck) (the double-array da)
    (setf base (resize base node-idx #x00) 
          chck (resize chck next-idx #xFF))
    
    (setf (aref base node-idx) base-idx
          (aref chck next-idx) arc)
    next-idx))

(defun build-from-trie-impl (trie alloca da node-idx)
  (declare #.dawg::*fastest*)
  (when (zerop (mod (the fixnum node-idx) 1000))
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
  (let ((da (make-double-array :base (make-array 0 :element-type 'uint4)
                               :chck (make-array 0 :element-type 'uint1)))) 
    (build-from-trie-impl trie (node-allocator:make) da 0)
    da))

(defun leaf? (node)
  (oddp node))

(defun member?-impl (in node base chck)
  (let ((next (+ (aref base node) (byte-stream:peek in))))
    (when (= (aref chck next) (byte-stream:peek in))
      (if (byte-stream:eos? in)
          t
        (and #+C (not (leaf? node))
             (member?-impl (byte-stream:eat in) next base chck))))))

(defun member?(key da)
  (with-slots (base chck) (the double-array da)
    (member?-impl (byte-stream:make (dawg::string-to-octets key))
                  0
                  base chck)))

(dawg::package-alias :dawg.byte-stream)
(dawg::package-alias :dawg.double-array.node-allocator)
