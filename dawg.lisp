(defpackage dawg
  (:use :common-lisp :dawg.global)
  (:shadow :common-lisp load)
  (:export build
           load
           member?
           get-id
           each-common-prefix))
(in-package :dawg)

(package-alias :dawg.octet-stream :stream)

;;;;;;;;;;;;;;;;;;;;
;;; special variable
(eval-when (:compile-toplevel)
  (defvar *args-type* '(simple-characters double-array &key (:start positive-fixnum)
                                                            (:end positive-fixnum))))
;;;;;;;;;;;;;;;
;;; declamation
(declaim (inline terminal? sibling-total inc-id each-common-prefix-impl)
         (ftype (function #.*args-type* boolean) member?)
         (ftype (function #.*args-type* (or null positive-fixnum)) get-id))

;;;;;;;;;;;;;;;;
;;; double-array
(defstruct double-array
  (base #() :type (simple-array uint4))
  (opts #() :type (simple-array uint4))
  (chck #() :type (simple-array uint1)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function(2)
(defun terminal? (opts node)
  (ldb-test (byte 1 0) (aref opts node)))

(defun sibling-total (opts node)
  (ash (aref opts node) -1))

(defun inc-id (id opts node)
  (+ id (if (terminal? opts node) 1 0) (sibling-total opts node)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; external function(2)
(defun member? (key double-array &key (start 0) (end (length key)))
  (declare #.*interface*)
  (with-slots (base chck opts) double-array
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

(defun get-id (key double-array &key (start 0) (end (length key)))
  (declare #.*interface*)
  (with-slots (base chck opts) double-array
    (declare #.*fastest*)
    (let ((in (stream:make key :start start :end end)))
      (declare (dynamic-extent in))
      (nlet recur ((node 0) (id 0))
        (declare (positive-fixnum id))
        (if (stream:eos? in)
            (and (terminal? opts node) id)
          (let* ((arc (stream:read in))
                 (next (the uint4 (+ (aref base node) arc))))
            (when (= (aref chck next) arc)
              (recur next (inc-id id opts node)))))))))

(defmacro each-common-prefix ((match-id match-end)
                              (key double-array &key (start 0) (end `(length ,key)))
                              &body body)
  `(progn
     (each-common-prefix-impl 
      (lambda (,match-id ,match-end)
        (declare (positive-fixnum ,match-id)
                 (array-index ,match-end))
        ,@body)
      ,key ,double-array ,start ,end)
     t))

(defun each-common-prefix-impl (fn key double-array start end)
  (declare #.*interface*
           (function fn)
           (simple-characters key)
           (double-array double-array)
           (positive-fixnum start end))
  (with-slots (base chck opts) double-array
    (declare #.*fastest*)
    (let ((in (stream:make key :start start :end end)))
      (declare (dynamic-extent in))
      (nlet recur ((node 0) (id 0))
        (declare (positive-fixnum id))
        (when (terminal? opts node)
          (funcall fn id (stream:position in)))
        (if (stream:eos? in)
            (and (terminal? opts node) id)
          (let* ((arc (stream:read in))
                 (next (the uint4 (+ (aref base node) arc))))
            (when (= (aref chck next) arc)
              (recur next (inc-id id opts node)))))))))

(package-alias :dawg.octet-stream)
