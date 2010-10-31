(defpackage dawg.double-array-builder
  (:use :common-lisp :dawg.global)
  (:export build-from-bintrie))
(in-package :dawg.double-array-builder)

(package-alias :dawg.double-array.node-allocator :node-allocator)
(package-alias :dawg.double-array.buffered-output :output)
(package-alias :dawg.bintrie-builder :bintrie)

;;;;;;;;;;;;;;;
;;; declamation
(declaim #.*fastest*
         (inline set-base set-chck set-opts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; da (abbreviation of "double array")
(defstruct da
  (base t :type output:buffered-output)
  (chck t :type output:buffered-output)
  (opts t :type output:buffered-output)
  (done-count 0 :type positive-fixnum))

;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function
(defun temp-path (base ext)
  (format nil "~A.~A.~A" base ext (gentemp)))

(defun concat-files (output-filepath &rest input-filepaths)
  (muffle
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

;;;;;;;;;;;;;;;;;;
;;; build function
(defun set-base (da node-idx base-idx)
  (declare (positive-fixnum node-idx base-idx))
  (with-slots (base) (the da da)
    (output:write-uint base-idx base :position node-idx)))

(defun set-chck (da base-idx arc &aux (next-idx (+ base-idx arc)))
  (declare (positive-fixnum base-idx next-idx)
           (octet arc))
  (with-slots (chck) (the da da)
    (output:write-uint arc chck :position next-idx))
  next-idx)

(defun set-opts (da node-idx options)
  (declare (positive-fixnum node-idx options))
  (output:write-uint options (da-opts da) :position node-idx))
           
(defun build-impl (trie alloca da node-idx memo show-progress elem-count)
  (a.if #1=(gethash (bintrie:node-child trie) memo)
        (progn
          (when show-progress
            (incf (da-done-count da) (bintrie:element-count trie)))
          (set-opts da node-idx (bintrie:node-options trie))
          (set-base da node-idx it))
    (let ((children (bintrie:collect-children trie)))
      (set-opts da node-idx (bintrie:node-options trie))
      (when children
        (let ((base-idx (node-allocator:allocate
                         alloca 
                         (mapcar #'bintrie:node-label children))))
          (setf #1# base-idx)
          (set-base da node-idx base-idx)
          (dolist (child children)
            (when (and show-progress (bintrie:node-terminal? child))
              (incf (da-done-count da))
              (when (zerop (mod (da-done-count da) 100000))
                (format t ";   ~A/~A (~,1F%)~%" 
                        #2=(da-done-count da) #3=elem-count (muffle (* (/ #2# #3#) 100)))))
            (build-impl child alloca da
                        (set-chck da base-idx (bintrie:node-label child))
                        memo show-progress elem-count)))))))

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defun build-from-bintrie (trie &key output-file show-progress)
  (when show-progress
    (format t "~2&; build double array from trie:~%"))
  (let ((base-path (temp-path output-file "base"))
        (opts-path (temp-path output-file "opts"))
        (chck-path (temp-path output-file "chck")))
    (when show-progress
      (format t ";  create tmpfiles: ~A, ~A, ~A~%" base-path opts-path chck-path))
    (unwind-protect
        (progn
          (when show-progress
            (format t "; build:~%"))
          (output:with-output (base base-path :byte-width 4)
            (output:with-output (opts opts-path :byte-width 4)
              (output:with-output (chck chck-path :byte-width 1)
                (let ((da (make-da :base base :chck chck :opts opts)))
                  (build-impl trie (node-allocator:make) da 0 
                              (make-hash-table :test #'eq)
                              show-progress (bintrie:element-count trie))))))
          (when show-progress
            (format t "; concatenate tempfiles to ~A~%"  output-file))
          (concat-files output-file base-path opts-path chck-path))
      (loop FOR path IN (list base-path opts-path chck-path)
            WHEN (probe-file path)
        DO
        (delete-file path)))))

(package-alias :dawg.double-array.node-allocator)
(package-alias :dawg.double-array.buffered-output)
(package-alias :dawg.bintrie-builder)
