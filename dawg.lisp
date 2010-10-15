(in-package :dawg)
(package-alias :dawg.byte-stream :byte-stream)

(declaim (inline make-node))

(defstruct node
  (label     0 :type octet)
  (sibling nil :type (or null node))
  (child   nil :type (or null node))
  (hash     -1 :type fixnum))

(defun push-child (in parent)
  (declare #.*fastest*) 
  (let ((new-node (make-node :label (byte-stream:peek in))))
    (setf (node-sibling new-node) (node-child parent)
          (node-child parent) new-node)
    (unless (byte-stream:eos? in)
      (push-child (byte-stream:eat in) new-node))))

(defun insert (in parent)
  (declare #.*fastest*)
  (let ((node (node-child parent)))
    (if (or (null node)
            (/= (byte-stream:peek in) (node-label node)))
        (push-child in parent)
      (insert (byte-stream:eat in) node))))

(defun build-from-file (filepath)
  (declare #.*fastest*)
  (let* ((trie (make-node))
         (cnt 0))
    (declare (fixnum cnt))
    (each-file-line-bytes (bytes beg end filepath)
      (when (zerop (mod (incf cnt) 5000))
        (print (list cnt)))
      (let ((in (byte-stream:make bytes :start beg :end end)))
        (declare (dynamic-extent in))
        (insert in trie)))
    trie))

#|
(defun member?-impl (in parent)
  (loop FOR node IN (node-children parent)
    DO
    (cond ((> (byte-stream:peek in) (node-label node))
           (return nil))
          ((= (byte-stream:peek in) (node-label node))
           (return (or (byte-stream:eos? in)
                       (member?-impl (byte-stream:eat in) node)))))))

(defun member? (key trie)
  (let ((in (byte-stream:make (string-to-octets key))))
    (member?-impl in trie)))
|#

(defun node-count-impl (node memo)
  (when (and node (not (gethash node memo)))
    (setf (gethash node memo) t)
    (node-count-impl (node-child node) memo)
    (node-count-impl (node-sibling node) memo)))

(defun node-count (trie)
  (let ((memo (make-hash-table :test #'eq)))
    (node-count-impl trie memo)
    (hash-table-count memo)))

(package-alias :dawg.byte-stream)
