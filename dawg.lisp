(in-package :dawg)

(package-alias :dawg.octet-stream :stream)

(defun build (&key input-file (output-file "") show-progress) ;; XXX:
  (declare #.*interface*
           ((or string pathname) input-file output-file))
  (declare (ignorable output-file))
  (dawg.bintrie-builder:build-from-file input-file :show-progress show-progress))

(defstruct double-array
  (base #() :type (simple-array uint4))
  (opts #() :type (simple-array uint4))
  (chck #() :type (simple-array uint1)))

(defun read-array-sizes (index-path)
  (with-open-file (in index-path :element-type 'uint1)
    (values (read-bigendian-uint4 in)
            (read-bigendian-uint4 in)
            (read-bigendian-uint4 in))))

(defun read-array (index-path &key size element-type offset)
  (with-open-file (in index-path :element-type element-type)
    (file-position in offset)
    (let ((ary (make-array size :element-type element-type)))
      (read-sequence ary in)
      ary)))

(defun load (index-path)
  (declare ((or string pathname file-stream) index-path))
  (multiple-value-bind (base-size opts-size chck-size)
                       (read-array-sizes index-path)
    (make-double-array 
     :base (read-array index-path :size (/ base-size 4)
                                  :element-type 'uint4 
                                  :offset 3)
     :opts (read-array index-path :size (/ opts-size 4)
                                  :element-type 'uint4
                                  :offset (+ 3 (/ base-size 4)))
     :chck (read-array index-path :size chck-size
                                  :element-type 'uint1
                                  :offset (+ 12 base-size opts-size)))))

(defun terminal? (da node)
  (with-slots (opts) (the double-array da)
    (ldb-test (byte 1 0) (aref opts node))))

(defun sibling-total (da node)
  (with-slots (opts) (the double-array da)
    (ash (aref opts node) -1)))

(defun member?-impl (in da node)
  (with-slots (base chck opts) (the double-array da)
    (if (stream:eos? in)
        (terminal? da node)
      (let* ((arc (stream:read in))
             (next (+ (aref base node) arc)))
        ;;(print (list node arc :-> next :<- (aref chck next) :terminal (terminal? da node)))
        (when (= (aref chck next) arc)
          (member?-impl in da next))))))
     
;; TODO: nlet 
(defun member? (key double-array)
  (let ((in (stream:make key)))
    (declare (dynamic-extent in))
    (member?-impl in double-array 0)))

(defun get-id-impl (in da node id)
  (with-slots (base chck opts) (the double-array da)
    (if (stream:eos? in)
        (and (terminal? da node) id)
      (let* ((arc (stream:read in))
             (next (+ (aref base node) arc)))
        (when (= (aref chck next) arc)
          (get-id-impl in da next 
                       (+ id (if (terminal? da node) 1 0) (sibling-total da node))))))))

;; TODO: start, end
(defun get-id (key double-array)
  (let ((in (stream:make key)))
    (declare (dynamic-extent in))
    (get-id-impl in double-array 0 0)))

(defun each-common-prefix (fn key da)
  (labels ((recur (in node id)
             (if (stream:eos? in)
                 (when (terminal? da node)
                   (funcall fn id (stream:position in)))
               (with-slots (base chck opts) (the double-array da)
                 (when (terminal? da node)
                   (funcall fn id (stream:position in)))
                 (let* ((arc (stream:read in))
                        (next (+ (aref base node) arc)))
                   (when (= (aref chck next) arc)
                     (recur in next
                            (+ id (if (terminal? da node) 1 0) (sibling-total da node)))))))))
    (let ((in (stream:make key)))
      (declare (dynamic-extent in))
      (recur in 0 0))))

;;(defmacro each-common-prefix ((id end) (key #|start end|#) &body body)

(package-alias :dawg.octet-stream)