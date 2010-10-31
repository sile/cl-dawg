(defpackage dawg.double-array-builder
  (:use :common-lisp :dawg.global)
  (:export build-from-bintrie))
(in-package :dawg.double-array-builder)

(declaim #.*fastest*)

(package-alias :dawg.double-array.node-allocator :node-allocator)
(package-alias :dawg.double-array.buffered-output :output)
(package-alias :dawg.bintrie-builder :bintrie)

(defstruct da
  (base t :type output:buffered-output)
  (chck t :type output:buffered-output)
  (opts t :type output:buffered-output))

(defun set-base-node (da node-idx base-idx)
  (declare (positive-fixnum node-idx base-idx))
  (with-slots (base) (the da da)
    (output:write-uint base-idx base :position node-idx)))

(defun set-chck-node (da base-idx arc &aux (next-idx (+ base-idx arc)))
  (declare (positive-fixnum base-idx next-idx)
           (octet arc))
  (with-slots (chck) (the da da)
    (output:write-uint arc chck :position next-idx))
  next-idx)

;; TODO: rename
(defun set-opts-node (da node-idx options)
  (declare (positive-fixnum node-idx options))
  (output:write-uint options (da-opts da) :position node-idx))
           

;; TODO: show-progress
(defun build-from-bintrie-impl (trie alloca da node-idx memo)
  (a.if #1=(gethash (bintrie:node-child trie) memo)
        (progn
          (set-opts-node da node-idx (bintrie:node-options trie))
          (set-base-node da node-idx it))
    (let ((children (bintrie:collect-children trie)))
      (set-opts-node da node-idx (bintrie:node-options trie))
      (when children
        (let ((base-idx (node-allocator:allocate
                         alloca 
                         (mapcar #'bintrie:node-label children))))
          (setf #1# base-idx)
          (set-base-node da node-idx base-idx)
          (dolist (child children)
            (build-from-bintrie-impl 
             child alloca da
             (set-chck-node da base-idx (bintrie:node-label child))
             memo)))))))

(defun temp-path (base ext)
  (format nil "~A.~A.~A" base ext (gentemp)))

(defun concat-files (output-filepath &rest input-filepaths)
  (muffle-compiler-note
   (with-open-file (out output-filepath :element-type 'octet
                                        :direction :output
                                        :if-exists :supersede)
     (dolist (input-filepath input-filepaths)
       (with-open-file (in input-filepath :element-type 'octet)
         (write-bigendian-uint4 (file-length in) out)))

     (dolist (input-filepath input-filepaths)
       (with-open-file (in input-filepath :element-type 'octet)
         (let ((buf (make-array (file-length in) :element-type 'octet)))
           (read-sequence buf in)
           (write-sequence buf out)))))))

(defun build-from-bintrie (trie &key output-file)
  (let ((base-path (temp-path output-file "base"))
        (opts-path (temp-path output-file "opts"))
        (chck-path (temp-path output-file "chck")))
    (unwind-protect
        (progn
          (output:with-output (base base-path :byte-width 4)
            (output:with-output (opts opts-path :byte-width 4)
              (output:with-output (chck chck-path :byte-width 1)
                (let ((da (make-da :base base :chck chck :opts opts)))
                  (build-from-bintrie-impl trie
                                           (node-allocator:make)
                                           da 0 
                                           (make-hash-table :test #'eq))))))
          (concat-files output-file base-path opts-path chck-path))
      (loop FOR path IN (list base-path opts-path chck-path)
            WHEN (probe-file path)
        DO
        (delete-file path))))
  t)

(package-alias :dawg.double-array.node-allocator)
(package-alias :dawg.double-array.buffered-output)
(package-alias :dawg.bintrie-builder)
