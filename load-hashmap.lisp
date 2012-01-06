(eval-when (:compile-toplevel)
  (defun load-local-system (package &optional (package-directory #P"./"))
    (let #.`((asdf:*central-registry* (directory package-directory))
             ;; or #+ASDF2
             ,@(when #.#1=(find-symbol "*DEFAULT-SOURCE-REGISTRIES*" :asdf)
                     `((,#1# nil))))
         (asdf:load-system package))))

(load-local-system :dict-0.2.0 
  #.(merge-pathnames #P"lib/dict-0.2.0/" *compile-file-pathname*))
