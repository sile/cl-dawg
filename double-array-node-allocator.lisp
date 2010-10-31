(defpackage :dawg.double-array.node-allocator
  (:use :common-lisp :dawg.global)
  (:export make
           allocate))
(in-package :dawg.double-array.node-allocator)

(defconstant +BUFFER_SIZE+ 89120)

(deftype base-flag () `(simple-bit-vector ,+BUFFER_SIZE+))
(deftype nexts () `(simple-array fixnum (,+BUFFER_SIZE+)))

(defstruct node-allocator 
  (head #x100 :type fixnum)
  (bits   #*  :type base-flag)
  (nexts #()  :type nexts)
  (prevs #()  :type nexts)
  (offset  0  :type fixnum))

(defun make ()
  (let ((bits  (make-array +BUFFER_SIZE+ :element-type 'bit :initial-element 0))
        (nexts (make-array +BUFFER_SIZE+ :element-type 'fixnum))
        (prevs (make-array +BUFFER_SIZE+ :element-type 'fixnum)))
    (loop FOR i FROM 0 BELOW +BUFFER_SIZE+ 
      DO
      (setf (aref nexts i) (1+ i)
            (aref prevs i) (1- i)))
    (make-node-allocator :nexts nexts :prevs prevs :bits bits)))

(defun shift (alloca)
  (print :shift)
  (with-slots (bits nexts prevs offset head) (the node-allocator alloca)
    (let ((new-offset head))
      (loop WHILE (< new-offset (+ offset (- +BUFFER_SIZE+ (* #x100 2))))
        DO
        (setf new-offset (aref nexts (- new-offset offset))))
      (print `(:offset ,offset :-> ,new-offset))
      (let* ((delta (- new-offset offset))
             (use-len (- +BUFFER_SIZE+ delta)))
        (shiftf (subseq bits 0 use-len) (subseq bits delta))
        (fill bits 0 :start use-len)

        (setf offset new-offset)
        
        (shiftf (subseq nexts 0 use-len) (subseq nexts delta))
        (shiftf (subseq prevs 0 use-len) (subseq prevs delta))
        (loop FOR i FROM (+ offset use-len) BELOW (+ offset +BUFFER_SIZE+)
          DO
          (setf (aref nexts (- i offset)) (1+ i)
                (aref prevs (- i offset)) (1- i)))

        (setf head offset)
        (loop WHILE (< head (+ offset #x100))
          DO
          (setf head (aref nexts (- head offset)))))))
  alloca)

(defun ref (alloca index)
  (declare #.dawg::*fastest*
           (fixnum index))
  (with-slots (offset nexts) (the node-allocator alloca)
    (assert (<= offset index) () "offset:~A > index:~A" offset index)
    (if (<= (+ offset +BUFFER_SIZE+) index)
        (ref (shift alloca) index)
      (aref nexts (- index offset)))))

(defun bref (alloca index)
  (declare #.dawg::*fastest*
           (fixnum index))
  (with-slots (bits offset) (the node-allocator alloca)
    ;; (assert (<= offset index))
    (if (> offset index)
        1
    (if (<= (+ offset +BUFFER_SIZE+) index)
        (bref (shift alloca) index)
      (bit bits (- index offset)))))
  )

(defun get-next (alloca index)
  (ref alloca index))

(defun can-allocate? (alloca index arcs)
  (declare #.dawg::*fastest*
           (list arcs)
           (fixnum index))
  (and (zerop (bref alloca index))
       (every (lambda (arc)
                (/= -1 (ref alloca (+ index arc))))
              arcs)))

(defun allocate-impl (alloca index arcs)
  (declare #.dawg::*fastest*
           (fixnum index))
  ;;(print `(:alloc ,index ,arcs))
  (with-slots (bits head prevs nexts offset) (the node-allocator alloca)
    (when (<= offset index)
      (assert (<= offset index))
      (setf (bit bits (- index offset)) 1))
    (loop WITH base = index
          FOR arc OF-TYPE (mod #x100) IN arcs
          FOR index OF-TYPE fixnum = (+ base arc)
      DO
      (ref alloca index) ;; XXX:
      (macrolet ((prev (index) `(aref prevs (- ,index offset)))
                 (next (index) `(aref nexts (- ,index offset))))
        (assert (/= (prev index) -1))
        (assert (/= (next index) -1))
        ;;(print (list index (prev index) (next index)))
        ;; XXX:
        (when (= head index)
          (print `(:hit ,head))
          (setf head (next index)))

        (when (<= offset (prev index))
          (setf (next (prev index)) (next index)))

          (ref alloca index)
          (ref alloca (next index))
        (when (<= offset index)


          (setf (prev (next index)) (prev index)
                (prev index) -1
                (next index) -1))))))

(defun allocate (alloca arcs)
  (declare #.dawg::*fastest*)
  (with-slots (head) (the node-allocator alloca)
    (loop WITH front OF-TYPE (mod #x100) = (car arcs)
          FOR cur = (get-next alloca head) THEN (get-next alloca cur)
          FOR base OF-TYPE fixnum = (- cur front)
          UNTIL (and (plusp base) (can-allocate? alloca base (cdr arcs)))
      FINALLY
      (allocate-impl alloca base arcs)
      (return base))))
