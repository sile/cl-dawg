(defpackage :dawg.double-array.node-allocator
  (:use :common-lisp)
  (:export make
           allocate))
(in-package :dawg.double-array.node-allocator)

(defstruct node-allocator 
  (bits   #*0  :type bit-vector)
  (nexts #(1)  :type (simple-array fixnum))
  (prevs #(-1) :type (simple-array fixnum)))

(defun make ()
  (make-node-allocator :nexts (make-array 1 :element-type 'fixnum :initial-contents '(1))
                       :prevs (make-array 1 :element-type 'fixnum :initial-contents '(-1))))

(defun resize (alloca)
  (with-slots (bits nexts prevs) (the node-allocator alloca)
    (let ((old-len (length nexts))
          (new-len (* (length nexts) 2)))
      (setf bits  (adjust-array bits new-len :initial-element 0)
            nexts (adjust-array nexts new-len)
            prevs (adjust-array prevs new-len))
      (loop FOR i FROM old-len BELOW new-len
        DO
        (setf (aref nexts i) (1+ i)
              (aref prevs i) (1- i)))))
  alloca)

(defun get-next (alloca index)
  (with-slots (nexts) (the node-allocator alloca)
    (if (<= (length nexts) (+ index #x100))
        (get-next (resize alloca) index)
      (aref nexts index))))

(defun can-allocate? (alloca index arcs)
  (get-next alloca index) ; XXX:
  (with-slots (bits nexts) (the node-allocator alloca)
    (and (zerop (bit bits index))
         (every (lambda (arc) (>= (aref nexts (+ index arc)) 0)) arcs))))

(defun allocate-impl (alloca index arcs)
  (with-slots (bits nexts prevs) (the node-allocator alloca)
    (setf (bit bits index) 1)
    (loop WITH base = index
          FOR arc IN arcs
          FOR index = (+ base arc)
      DO
      (setf (aref nexts (aref prevs index)) (aref nexts index)
            (aref prevs (aref nexts index)) (aref prevs index)
            (aref nexts index) -1
            (aref prevs index) -1))))

(defun allocate (alloca arcs)
  (loop FOR cur = (get-next alloca 0) THEN  (get-next alloca cur)
        UNTIL (can-allocate? alloca cur arcs)
    FINALLY
    (allocate-impl alloca cur arcs)
    (return cur)))

