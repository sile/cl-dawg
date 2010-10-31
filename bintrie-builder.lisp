(defpackage dawg.bintrie-builder
  (:use :common-lisp :dawg.global)
  (:export build-from-file
           collect-children
           node-label
           node-child
           node-options
           member?))
(in-package :dawg.bintrie-builder)

(package-alias :dawg.octet-stream :stream)

(declaim #.*fastest*
         (inline make-node calc-child-total calc-sibling-total node-options))

(defstruct node
  (label         0 :type octet)
  (terminal?   nil :type boolean)
  (child       nil :type (or null node))
  (sibling     nil :type (or null node))
  (child-total   0 :type positive-fixnum)
  (sibling-total 0 :type positive-fixnum)
  (hash         -1 :type fixnum))

(defun node-options (node)
  (with-slots (terminal? sibling-total) (the node node)
    (+ (if terminal? 1 0)
       (ash sibling-total 1))))

(defun node= (n1 n2)
  (and (eq (node-child n1) (node-child n2))
       (eq (node-sibling n1) (node-sibling n2))
       (= (node-label n1) (node-label n2))
       (eq (node-terminal? n1) (node-terminal? n2))))

(macrolet ((calc-xxx-total (node slot)
             `(with-slots (,slot) (the node ,node)
                (if (null ,slot)
                    0
                  (the positive-fixnum
                       (+ (if (node-terminal? ,slot) 1 0)
                          (node-child-total ,slot) (node-sibling-total ,slot)))))))
  (defun calc-child-total (node) (calc-xxx-total node child))
  (defun calc-sibling-total (node) (calc-xxx-total node sibling)))

(defun sxhash-node (node)
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

(sb-ext:define-hash-table-test node= sxhash-node)

(defun share (node memo)
  (if (null node)
      nil
    (or (gethash node memo)
        (progn 
          (setf (node-child node) (share (node-child node) memo)
                (node-sibling node) (share (node-sibling node) memo))
          (gethash node memo))
        (setf (gethash node memo) node))))

(defun push-child (in parent)
  (if (stream:eos? in)
      (setf (node-terminal? parent) t)
    (let ((new-node (make-node :label (stream:read in))))
      (shiftf (node-sibling new-node) (node-child parent) new-node)
      (push-child in new-node))))

(defun insert (in parent memo)
  (let ((node (node-child parent)))
    (if (or (null node)
            (stream:eos? in)
            (/= (stream:peek in) (node-label node)))
        (progn
          (setf (node-child parent) (share node memo))
          (push-child in parent))
      (insert (stream:eat in) node memo))))

(defun build-from-file (filepath &key show-progress)
  (when show-progress
    (format t "~&; build trie from ~A:~%" filepath))
  (with-open-file (is filepath)
    (loop WITH trie = (make-node)
          WITH memo = (make-hash-table :test #'node=)
          FOR line-num OF-TYPE positive-fixnum FROM 0
          FOR line = (read-line is nil nil)
          WHILE line
      DO
      (when (and show-progress (zerop (mod line-num 100000)))
        (format t "~&;  ~A~%" line-num))
      (let ((in (stream:make line)))
        (declare (dynamic-extent in))
        (insert in trie memo))

      FINALLY
      (return (share trie memo)))))
 
(defun collect-children (node)
  (loop WITH acc = '()
        FOR child = (node-child node)
               THEN (node-sibling child)
        WHILE child
    DO
    (push child acc)
    FINALLY
    (return acc)))

;; for debug
(defun member?-impl (in node parent)
  (cond ((stream:eos? in) (node-terminal? parent))
        ((null node) nil)
        ((= (stream:peek in) (node-label node))
         (member?-impl (stream:eat in) (node-child node) node))
        ((< (stream:peek in) (node-label node))
         (member?-impl in (node-sibling node) parent))))

(defun member? (key trie)
  (declare #.*interface*
           (simple-characters key)
           (node trie))
  (let ((in (stream:make key)))
    (member?-impl in (node-child trie) trie)))

(package-alias :dawg.octet-stream)
