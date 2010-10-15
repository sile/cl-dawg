(in-package :dawg)
(package-alias :dawg.byte-stream :byte-stream)

(declaim (inline make-node))

(defstruct node
  (label     0  :type octet)
  (children '() :type list))

(defun push-child (in parent)
  (declare #.*fastest*) 
  (let ((new-node (make-node :label (byte-stream:peek in))))
    (push new-node (node-children parent))           
    (unless (byte-stream:eos? in)
      (push-child (byte-stream:eat in) new-node))))

(defun insert (in parent)
  (declare #.*fastest*)
  (let ((node (first (node-children parent))))
    (if (or (null node)
            (/= (byte-stream:peek in) (node-label node)))
        (push-child in parent)
      (insert (byte-stream:eat in) node))))

(defun build-from-file (filepath)
  (declare #.*fastest*)
  (let ((trie (make-node)))
    (each-file-line-bytes (bytes beg end filepath)
      (let ((in (byte-stream:make bytes :start beg :end end)))
        (declare (dynamic-extent in))
        (insert in trie)))
    trie))

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
     
(defun node-count (trie)
  (loop FOR child IN (node-children trie)
        SUM (1+ (node-count child))))


(package-alias :dawg.byte-stream)
