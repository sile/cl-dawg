(in-package :dawg)

(declaim (inline make-node calc-child-total calc-sibling-total))

(defstruct node
  (label    #\Null :type character)
  (sibling     nil :type (or null node))
  (child       nil :type (or null node))
  (terminal?   nil :type boolean)
  (child-total   0 :type positive-fixnum)
  (sibling-total 0 :type positive-fixnum)
  (hash         -1 :type fixnum))

(defun calc-child-total (node)
  (with-slots (child) (the node node)
    (if (null child)
        0
      (+ (if (node-terminal? child) 1 0)
         (node-child-total child) (node-sibling-total child)))))

(defun calc-sibling-total (node)
  (with-slots (sibling) (the node node)
    (if (null sibling)
        0
      (+ (if (node-terminal? sibling) 1 0)
         (node-child-total sibling) (node-sibling-total sibling)))))

(defun node= (n1 n2)
  (declare #.*fastest*)
  (and (eq (node-child n1) (node-child n2))
       (eq (node-sibling n1) (node-sibling n2))
       (char= (node-label n1) (node-label n2))
       (eq (node-terminal? n1) (node-terminal? n2))))

(defun sxhash-node (node)
  (declare #.*fastest*)
  (if (null node)
      #.(sxhash nil)
    (with-slots (hash child-total sibling-total) (the node node)
      (when (= -1 hash)
        (setf hash (logxor (sxhash (node-label node))
                           (sxhash (node-terminal? node))
                           (fixnumize (* (sxhash-node (node-child node)) 7))
                           (fixnumize (* (sxhash-node (node-sibling node)) 13))))
        (setf child-total (calc-child-total node)
              sibling-total (calc-sibling-total node)))
      hash)))

(defun share (node memo)
  (declare #.*fastest*)
  (if (null node)
      nil
    (or (gethash node memo)
        (progn 
          (setf (node-child node) (share (node-child node) memo)
                (node-sibling node) (share (node-sibling node) memo))
          (gethash node memo))
        (setf (gethash node memo) node))))

(defun push-child (in parent)
  (declare #.*fastest*) 
  (if (char-stream-eos? in)
      (setf (node-terminal? parent) t)
    (let ((new-node (make-node :label (char-stream-read in))))
      (setf (node-sibling new-node) (node-child parent)
            (node-child parent) new-node)
      (push-child in new-node))))

(defun insert (in parent memo)
  (declare #.*fastest*)
  (let ((node (node-child parent)))
    (if (or (null node)
            (char/= (char-stream-peek in) (node-label node)))
        (progn
          (setf (node-child parent) (share node memo))
          (push-child in parent))
      (insert (char-stream-eat in) node memo))))

(sb-ext:define-hash-table-test node= sxhash-node)
(defun build-from-file (filepath &key show-progress)
  (declare #.*interface*
           (string filepath))
  (with-open-file (is filepath)
    (declare #.*fastest*)
    (loop WITH trie = (make-node)
          WITH memo = (make-hash-table :test #'node=)
          FOR count OF-TYPE positive-fixnum FROM 0
          FOR line = (read-line is nil nil)
          WHILE line
      DO
      (when (and show-progress 
                 (zerop (mod count 10000)))
        (format t "~&; ~A~%" count))
      (let ((in (make-char-stream line)))
        (declare (dynamic-extent in))
        (insert in trie memo))

      FINALLY
      (return (share trie memo)))))

(defun member?-impl (in node parent)
  (declare #.*fastest*)
  (cond ((char-stream-eos? in)
         (node-terminal? parent))
        ((null node) nil)
        ((char= (char-stream-peek in) (node-label node))
         (member?-impl (char-stream-eat in) (node-child node) node))
        ((char< (char-stream-peek in) (node-label node))
         (member?-impl in (node-sibling node) parent))))

(defun member? (key trie)
  (declare #.*interface*
           (simple-characters key)
           (node trie))
  (locally 
   (declare #.*fastest*)
   (let ((in (make-char-stream key)))
    (member?-impl in (node-child trie) trie))))

(defun get-id-impl (in node parent id)
  (declare #.*fastest*
           (positive-fixnum id))
  (cond ((char-stream-eos? in)
         (and (node-terminal? parent) id))
        ((null node) nil)
        ((char= (char-stream-peek in) (node-label node))
         (get-id-impl (char-stream-eat in) (node-child node) node
                       (+ id (if (node-terminal? node) 1 0) (node-sibling-total node))))
        ((char< (char-stream-peek in) (node-label node))
         (get-id-impl in (node-sibling node) parent id))))

(defun get-id (key trie)
  (declare #.*interface*
           (simple-characters key)
           (node trie))
  (locally 
   (declare #.*fastest*)
   (let ((in (make-char-stream key)))
     (get-id-impl in (node-child trie) trie 0))))

#|
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
|#

(defun collect-children (node)
  (declare #.*fastest*)
  (nreverse
   (loop FOR child = (node-child node)
                THEN (node-sibling child)
         WHILE child
     COLLECT child)))
