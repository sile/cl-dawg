(defpackage :dawg.double-array
  (:use :common-lisp)
  (:export build-from-trie
           member?
           save))

(in-package :dawg.double-array)

(dawg::package-alias :dawg.byte-stream :byte-stream)
(dawg::package-alias :dawg.double-array.node-allocator :node-allocator)

(deftype uint4 () '(unsigned-byte 32))
(deftype uint1 () '(unsigned-byte 8))

(defstruct double-array 
  (base #() :type (simple-array uint4))
  (chck #() :type (simple-array uint1)))

(defun save (filepath da)
  (with-open-file (out filepath :direction :output
                                :if-exists :supersede
                                :element-type 'uint1)
    (with-slots (base chck) (the double-array da)
      (dawg::write-uint (length base) 4 out)
      (dawg::write-uint (length chck) 4 out)

      (loop FOR b ACROSS base
            DO (dawg::write-uint b 4 out))
      (loop FOR c ACROSS chck
            DO (dawg::write-uint c 1 out))))
  t)

(defun build-from-trie (trie)
  trie)

(defun leaf? (node)
  (oddp node))

(defun member?-impl (in node base chck)
  (let ((next (+ (aref base 0) (byte-stream:peek in))))
    (when (= (aref chck next) (byte-stream:peek in))
      (if (byte-stream:eos? in)
          t
        (and (not (leaf? node))
             (member?-impl (byte-stream:eat in) next base chck))))))

(defun member?(key da)
  (with-slots (base chck) (the double-array da)
    (member?-impl (byte-stream:make (dawg::string-to-octets key))
                  0
                  base chck)))

(dawg::package-alias :dawg.byte-stream)
(dawg::package-alias :dawg.double-array.node-allocator)