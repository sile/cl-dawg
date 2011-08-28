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

;;;;;;;;;;;;
;;; constant
(defconstant +BUFFER_SIZE+ 819200)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; da (abbreviation of "double array")
(defstruct da
  (node t :type output:buffered-output)
  (exts t :type stream)
  (done-count 0 :type positive-fixnum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; node
(defstruct node
  (index         0 :type positive-fixnum)
  (base          0 :type positive-fixnum)
  (type          0 :type (mod 4))
  (terminal?     t :type boolean)
  (sibling-total 0 :type positive-fixnum)
  (chck          0 :type uint1)
  (children    '() :type list))

(defun new-node (parent-base-idx trie)
  (declare (positive-fixnum parent-base-idx))
  (make-node :index (+ parent-base-idx (bintrie:node-label trie))
             :sibling-total #1=(bintrie:node-sibling-total trie)
             :terminal? (bintrie:node-terminal? trie)
             :chck (bintrie:node-label trie)
             :type (cond ((< #1# #x100) 0)
                         ((< #1# #x10000) 1)
                         ((< #1# #x1000000) 2)
                         (t 3))))

(defun child-acceptable-p (node)
  (with-slots (type children) (the node node)
    (let ((capacity (case type
                      (0 2)
                      (1 1)
                      (otherwise 0))))
      (plusp (- capacity (length children))))))

(defun add-child (node child)
  (with-slots (children) (the node node)
    (setf children (nconc children (list (bintrie:node-label child))))))

;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function
(defun merge-files (destination &rest files)
  ;; write each file size
  (with-open-output-file (out destination 'uint4)
    (loop FOR file IN files
          DO (with-open-file (in file :element-type 'uint1)
               (write-byte (file-length in) out))))
  
  ;; write each file content
  (with-open-output-file (out destination 'uint1 :if-exists :append)
    (loop FOR file IN files
          DO (with-open-file (in file :element-type 'uint1)
               (loop FOR b = (read-byte in nil nil)
                     WHILE b
                     DO (write-byte b out)))))
  (mapc #'delete-file files))


;;;;;;;;;;;;;;;;;;
;;; build function
(defun write-node-impl (node da)
  (with-slots (index type base terminal? sibling-total chck children) (the node node)
    (let ((n 0))
      (declare ((unsigned-byte 64) n))
      (muffle
       (setf (ldb (byte 29  0) n) base
             (ldb (byte  2 29) n) type
             (ldb (byte  1 31) n) (if terminal? 1 0)
             (ldb (byte  8 32) n) chck)
       (case type
         (0
          (setf (ldb (byte  8 40) n) (or (first children) 0)
                (ldb (byte  8 48) n) (or (second children) 0)
                (ldb (byte  8 56) n) sibling-total))
         (1 
          (setf (ldb (byte  8 40) n) (or (first children) 0)
                (ldb (byte 16 48) n) sibling-total))
         (2 
          (setf (ldb (byte 24 40) n) sibling-total))
         (3 
          (setf (ldb (byte 24 40) n) (file-position (da-exts da)))
          (write-byte sibling-total (da-exts da))))

       (output:write-uint n (da-node da) :position index)))))

(defun write-node (node da &key base)
  (when base
    (setf (node-base node) base))
  (write-node-impl node da))

(defun build-impl (trie alloca da node memo &optional show-progress elem-count)
  (declare (ignore show-progress elem-count))
  (a.if #1=(gethash (bintrie:node-child trie) memo)
        (write-node node da :base it)
    (let ((children (bintrie:collect-children trie)))
      (if (null children)
          (write-node node da)
        (progn
          (loop WHILE (and (null (cdr children))
                           (not (bintrie::node-terminal? (car children)))
                           (child-acceptable-p node))
            DO
            (add-child node (car children))
            (setf trie (car children))
            (setf children (bintrie:collect-children trie)))

          (a.if #1#
                (write-node node da :base it)
            (let ((base-idx (node-allocator:allocate
                             alloca 
                             (mapcar #'bintrie:node-label children))))
              (setf #1# base-idx)
              (write-node node da :base base-idx)
            
              (dolist (child children)
                (build-impl child alloca da (new-node base-idx child) memo)))))))))
#+C
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
(defmacro show (fmt &rest args)
  `(when show-progress
     (format t ,fmt ,@args)))

(defun build-from-bintrie (trie &key output-file show-progress)
  (show "~2&; build double array from trie:~%")
  (let ((node-file (format nil "~a.node" output-file))
        (exts-file (format nil "~a.ext" output-file)))
    (show ";  create tmpfiles: ~a, ~a~%" node-file exts-file)

    (show "; build:~%")
    (output:with-output (node node-file :byte-width 8)
      (with-open-output-file (exts exts-file 'uint4)
        (let ((da (make-da :node node :exts exts)))
          (build-impl trie (node-allocator:make) da 
                      (new-node 0 trie)
                      (make-hash-table :test #'eq)))))
    (show "; concatenate tempfiles to ~A~%"  output-file)
    (merge-files output-file node-file exts-file))
  'done)

(package-alias :dawg.double-array.node-allocator)
(package-alias :dawg.double-array.buffered-output)
(package-alias :dawg.bintrie-builder)
