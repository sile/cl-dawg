(in-package :dawg)

(defun build (&key input-file (output-file "") show-progress) ;; XXX:
  (declare #.*interface*
           ((or string pathname file-stream) input-file output-file))
  (declare (ignorable output-file))
  (dawg.bintrie-builder:build-from-file input-file :show-progress show-progress))

