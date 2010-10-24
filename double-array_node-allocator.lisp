(defpackage :dawg.double-array.node-allocator
  (:use :common-lisp)
  (:export make
           allocate))
(in-package :dawg.double-array.node-allocator)

(defstruct node-allocator 
  (head     0  :type fixnum)
  (bits   #*0  :type simple-bit-vector)
  (nexts #(1)  :type (simple-array fixnum))
  (prevs #(-1) :type (simple-array fixnum)))

(defun make ()
  (make-node-allocator :nexts (make-array 1 :element-type 'fixnum :initial-contents '(1))
                       :prevs (make-array 1 :element-type 'fixnum :initial-contents '(-1))))

(defun resize (alloca)
  (declare #.dawg::*fastest*)
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
  (declare #.dawg::*fastest*
           (fixnum index))
  (with-slots (nexts) (the node-allocator alloca)
    (if (<= (length nexts) (+ index #x100))
        (get-next (resize alloca) index)
      (aref nexts index))))

(defun can-allocate? (alloca index arcs)
  (declare #.dawg::*fastest*
           (list arcs)
           (fixnum index))
  (get-next alloca index) ; XXX:
  (with-slots (bits nexts) (the node-allocator alloca)
    (and (zerop (bit bits index))
         (every (lambda (arc) (declare (fixnum arc)) (>= (aref nexts (+ index arc)) 0)) arcs))))

(defun allocate-impl (alloca index arcs)
  (declare #.dawg::*fastest*
           (fixnum index))
  (with-slots (bits nexts prevs) (the node-allocator alloca)
    (setf (bit bits index) 1)
    (loop WITH base = index
          FOR arc OF-TYPE (mod #x100) IN arcs
          FOR index OF-TYPE fixnum = (+ base arc)
      DO
      (setf (aref nexts (aref prevs index)) (aref nexts index)
            (aref prevs (aref nexts index)) (aref prevs index)
            (aref nexts index) -1
            (aref prevs index) -1))))

(defun allocate (alloca arcs)
  (declare #.dawg::*fastest*)
  (with-slots (head) (the node-allocator alloca)
    (loop WITH front OF-TYPE (mod #x100) = (car arcs)
          FOR cur = (get-next alloca head) THEN (get-next alloca cur)
          FOR base OF-TYPE fixnum = (- cur front)
          FOR cnt OF-TYPE fixnum FROM 0
          UNTIL (and (plusp base) (can-allocate? alloca base (cdr arcs)))
      FINALLY
      (when (> cnt #x200)
        (setf head (get-next alloca head)))
      (allocate-impl alloca base arcs)
      (return base))))
