(mac implicit (name (o val))
  `(do (defvar ,name (scheme.make-parameter ,val))
       (mac ,(sym (string "w/" name)) (v . body)
         (w/uniq (param gv gf)
           `(with (,param (defvar-impl ,',name)
                   ,gv ,v
                   ,gf (fn () ,@body))
              (scheme (parameterize ((,param ,gv)) (,gf))))))))
