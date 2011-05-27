(defpackage dawg
  (:use :common-lisp :dawg.global)
  (:shadow :common-lisp load)
  (:export dawg
           build
           load
           member?
           get-id
           each-common-prefix
           each-predictive))
(in-package :dawg)

(package-alias :dawg.octet-stream :stream)

;;;;;;;;;;;;;;;;;;;;
;;; special variable
(eval-when (:compile-toplevel)
  (defvar *args-type* '(simple-characters dawg &key (:start positive-fixnum)
                                                    (:end positive-fixnum))))
(defconstant +ARC_LIMIT+ #x100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dawg (double-array format)
(defstruct dawg
  (base #() :type (simple-array uint4))
  (opts #() :type (simple-array uint4))
  (chck #() :type (simple-array uint1)))

(defmethod print-object ((o dawg) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "~A:~A" :node-count (length (dawg-base o)))))

;;;;;;;;;;;;;;;
;;; declamation
(declaim (inline terminal? sibling-total inc-id each-common-prefix-impl)
         (ftype (function #.*args-type* boolean) member?)
         (ftype (function #.*args-type* (or null positive-fixnum)) get-id))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function(1)
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

;;;;;;;;;;;;;;;;;;;;;;;;
;;; external function(1)
(defun build (&key input output show-progress) 
  (declare ((or string pathname) input output))
  (let ((trie (dawg.bintrie-builder:build-from-file input 
                                                    :show-progress show-progress)))
    (dawg.double-array-builder:build-from-bintrie trie
                                                  :output-file output
                                                  :show-progress show-progress))
  t)

(defun load (index-path)
  (declare ((or string pathname file-stream) index-path))
  (multiple-value-bind (base-size opts-size chck-size)
                       (read-array-sizes index-path)
    (make-dawg
     :base (read-array index-path :size (/ base-size 4)
                                  :element-type 'uint4 
                                  :offset 3)
     :opts (read-array index-path :size (/ opts-size 4)
                                  :element-type 'uint4
                                  :offset (+ 3 (/ base-size 4)))
     :chck (read-array index-path :size chck-size
                                  :element-type 'uint1
                                  :offset (+ 12 base-size opts-size)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function(2)
(defun terminal? (opts node)
  (ldb-test (byte 1 0) (aref opts node)))

(defun sibling-total (opts node)
  (ash (aref opts node) -1))

(defun inc-id (id opts node)
  (the uint4
       (+ id (if (terminal? opts node) 1 0) (sibling-total opts node))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; external function(2)
(defun member? (key dawg &key (start 0) (end (length key)))
  (declare #.*interface*)
  (with-slots (base chck opts) dawg
    (declare #.*fastest*)
    (let ((in (stream:make key :start start :end end)))
      (declare (dynamic-extent in))
      (nlet recur ((node 0))
        (if (stream:eos? in)
            (terminal? opts node)
          (let* ((arc (stream:read in))
                 (next (the uint4 (+ (aref base node) arc))))
            (when (= (aref chck next) arc)
              (recur next))))))))

(defun get-id (key dawg &key (start 0) (end (length key)))
  (declare #.*interface*)
  (with-slots (base chck opts) dawg
    (declare #.*fastest*)
    (let ((in (stream:make key :start start :end end)))
      (declare (dynamic-extent in))
      (nlet recur ((node 0) (id -1))
        (declare (fixnum id))
        (if (stream:eos? in)
            (and (terminal? opts node) (inc-id id opts node))
          (let* ((arc (stream:read in))
                 (next (the uint4 (+ (aref base node) arc))))
            (when (= (aref chck next) arc)
              (recur next (inc-id id opts node)))))))))

(defmacro each-common-prefix ((match-id match-end)
                              (key dawg &key (start 0) (end `(length ,key)))
                              &body body)
  `(progn
     (each-common-prefix-impl 
      (lambda (,match-id ,match-end)
        (declare (positive-fixnum ,match-id)
                 (array-index ,match-end))
        ,@body)
      ,key ,dawg ,start ,end)
     t))

(defun each-common-prefix-impl (fn key dawg start end)
  (declare #.*interface*
           (function fn)
           (simple-characters key)
           (dawg dawg)
           (positive-fixnum start end))
  (with-slots (base chck opts) dawg
    (declare #.*fastest*)
    (let ((in (stream:make key :start start :end end)))
      (declare (dynamic-extent in))
      (nlet recur ((node 0) (id -1))
        (declare (fixnum id))
        (when (terminal? opts node)
          (funcall fn (inc-id id opts node) (stream:position in)))
        (unless (stream:eos? in)
          (let* ((arc (stream:read in))
                 (next (the uint4 (+ (aref base node) arc))))
            (when (= (aref chck next) arc)
              (recur next (inc-id id opts node)))))))))

(defun traverse-descendant (fn dawg node id)
  (with-slots (base chck opts) dawg
    (when (terminal? opts node)
      (funcall fn (inc-id id opts node)))
    (loop FOR arc FROM 1 BELOW +ARC_LIMIT+ 
          FOR next = (+ (aref base node) arc)
          WHEN (= (aref chck next) arc)
      DO
      (traverse-descendant fn dawg next (inc-id id opts node)))))

(defun each-predictive (fn key dawg start end)
 (declare #.*interface*
           (function fn)
           (simple-characters key)
           (dawg dawg)
           (positive-fixnum start end))
   (with-slots (base chck opts) dawg
    (declare #.*fastest*)
    (let ((in (stream:make key :start start :end end)))
      (declare (dynamic-extent in))
      (nlet recur ((node 0) (id -1))
        (declare (fixnum id))
;        (when (terminal? opts node)
;          (funcall fn (inc-id id opts node) (stream:position in)))
        (if (stream:eos? in)
            (traverse-descendant fn dawg node id) 
          (let* ((arc (stream:read in))
                 (next (the uint4 (+ (aref base node) arc))))
            (when (= (aref chck next) arc)
              (recur next (inc-id id opts node)))))))))


(package-alias :dawg.octet-stream)
