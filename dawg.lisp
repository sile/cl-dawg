(in-package :dawg)
(package-alias :dawg.byte-stream :byte-stream)
(package-alias :dawg.double-array :double-array)

(declaim (inline make-node))

(defstruct node
  (label     0 :type octet)
  (sibling nil :type (or null node))
  (child   nil :type (or null node))
  (hash     -1 :type fixnum))

(defun node= (n1 n2)
  (declare #.*fastest*)
  (and (= (node-label n1) (node-label n2))
       (eq (node-child n1) (node-child n2))
       (eq (node-sibling n1) (node-sibling n2))))

(defun sxhash-node (node)
  (declare #.*fastest*)
  (if (null node)
      #.(sxhash nil)
    (with-slots (hash) (the node node)
      (when (= -1 hash)
        (setf hash (logxor (sxhash (node-label node))
                           (fixnumize (* (sxhash-node (node-child node)) 7))
                           (fixnumize (* (sxhash-node (node-sibling node)) 13)))))
      hash)))

(defun memoize (node memo)
  (declare #.*fastest*)
  (if (null node)
      nil
    (or (gethash node memo)
        (progn 
          (setf (node-child node) (memoize (node-child node) memo)
                (node-sibling node) (memoize (node-sibling node) memo))
          (gethash node memo))
        (setf (gethash node memo) node))))

(defun push-child (in parent)
  (declare #.*fastest*) 
  (let ((new-node (make-node :label (byte-stream:peek in))))
    (setf (node-sibling new-node) (node-child parent)
          (node-child parent) new-node)
    (unless (byte-stream:eos? in)
      (push-child (byte-stream:eat in) new-node))))

(defun insert (in parent memo)
  (declare #.*fastest*)
  (let ((node (node-child parent)))
    (if (or (null node)
            (/= (byte-stream:peek in) (node-label node)))
        (progn
          (setf (node-child parent) (memoize node memo))
          (push-child in parent))
      (insert (byte-stream:eat in) node memo))))

(sb-ext:define-hash-table-test node= sxhash-node)
(defun build-from-file (filepath)
  (declare #.*fastest*)
  (let* ((trie (make-node))
         (memo (make-hash-table :test #'node=))
         (cnt 0))
    (declare (fixnum cnt))
    (each-file-line-bytes (bytes beg end filepath)
      (when (zerop (mod (incf cnt) 5000))
        (print (list (hash-table-count memo) cnt)))
      (let ((in (byte-stream:make bytes :start beg :end end)))
        (declare (dynamic-extent in))
        (values (insert in trie memo))))
    (setf trie (memoize trie memo))
    trie))

(defun member?-impl (in node)
  (declare #.*fastest*)
  (cond ((null node) nil)
        ((= (byte-stream:peek in) (node-label node))
         (or (byte-stream:eos? in)
             (member?-impl (byte-stream:eat in) (node-child node))))
        ((< (byte-stream:peek in) (node-label node))
         (member?-impl in (node-sibling node)))))

(defun member? (key trie)
  (declare #.*fastest*)
  (let ((in (byte-stream:make (string-to-octets key))))
    (member?-impl in (node-child trie))))

(defun node-count-impl (node memo)
  (when (and node (not (gethash node memo)))
    (setf (gethash node memo) t)
    (node-count-impl (node-child node) memo)
    (node-count-impl (node-sibling node) memo)))

(defun node-count (trie)
  (let ((memo (make-hash-table :test #'eq)))
    (node-count-impl trie memo)
    (hash-table-count memo)))

(defun save-as-double-array (filepath trie)
  (double-array:save filepath
                     (double-array:build-from-trie trie))
  t)

(defun collect-children (node)
  (declare #.*fastest*)
  (nreverse
   (loop FOR child = (node-child node)
                THEN (node-sibling child)
         WHILE child
     COLLECT child)))

(package-alias :dawg.byte-stream)
(package-alias :dawg.double-array)
