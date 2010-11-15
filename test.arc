(def an-err (error-msg)
  (list 'err-QFhlw05j4y error-msg))

(mac catcherr body
  `(on-err
    (fn (e) (an-err (details e)))
    (fn () ,@body)))

(set test-verbose*)
(set runtests*)

(def test-iso3 (desc result expected)
  (if (errsafe:iso expected result)
       (when test-verbose*
         (do (pr "ok " desc " => ")
             (write result)
             (prn)))
       (do (pr "FAIL " desc " => ")
           (write result)
           (pr ", not the expected result ")
           (write expected)
           (prn)
           (err "test failed"))))

(mac test-iso2 (expr expected)
  `(test-iso3 ',expr ,expr ,expected))

(mac test-iso args
  (when runtests*
    (if (is len.args 2)
         `(test-iso2 ,args.0 ,args.1)
         `(test-iso3 ,args.0 ,args.1 ,args.2))))

(mac test (expected expr (o qexpr expr))
  `(test-iso (tostring (write ',qexpr)) (catcherr ,expr) ,expected))

(mac testf (input f qf expected)
  (w/uniq (gexpected gresult)
    `(with (,gexpected  ,expected
            ,gresult (catcherr (,f ,input)))
       (test-iso (tostring (write ',input) (pr " ") (write ',qf))
                   ,gresult ,gexpected))))
