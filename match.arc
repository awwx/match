
(implicit pos*)
(implicit fail)

; failf must be called *outside* the w/fail scope,
; thus the code to pass the normal/fail result of calling f
; out through the w/fail

(def onparse-impl (parser successf failf)
  (with (return-value nil failed nil)
    (catch
      (w/fail (fn ()
                     (set failed)
                     (throw nil))
        (= return-value (parser))))
    (if failed
        (failf)
        (successf return-value))))

(mac onparse (var parser successf onfail)
  `(onparse-impl (fn () ,parser)
                 (fn (,var) ,successf)
                 (fn () ,onfail)))

(mac onfail (failf . body)
  (w/uniq gv
    `(onparse ,gv (do ,@body)
       ,gv
       ,failf)))

(def at-impl (parser)
  (let mark pos*
    (after (parser)
           (= pos* mark))))

(mac at body
  `(at-impl (fn () ,@body)))

(def try-impl (parser successf failf)
  (let mark pos*
    (onparse r (parser)
      (successf r)
      (do (= pos* mark)
          (failf)))))

(mac try (v parser successf failf)
  `(try-impl (fn () ,parser)
             (fn (,v) ,successf)
             (fn () ,failf)))

(def ascons (x)
  (if (or (no x) (acons x))
       x
      (isa x 'input)
       (drain (readc x))
      (isa x 'string)
       (coerce x 'cons)
       (err "don't know how to parse" x)))

(def fparse (seq body)
  (onfail nil
    (w/pos* (ascons seq)
      (body))))

(mac match (s . body)
  `(fparse ,s (fn () ,@body)))

(mac test-matchpos (input parser expected)
  `(test (ascons ,expected)
         (match ,input (do ,parser) pos*)))


(mac test-match1 (str parser expected)
  `(testf ,str [match _ (onfail '<<parse-fail>> ,parser)] ,parser ,expected))
                
(mac test-match (parser . body)
  `(do ,@(map (fn ((input expected))
                `(test-match1 ,input ,parser ,expected))
              (pair body))))

; TODO fail this?
(def at-end ()
  (no pos*))

; TODO rename to not-at-end?
(def fail-at-end ()
  (if (at-end) (fail)))

(def next ()
  (fail-at-end)
  (do1 car.pos*
       (= pos* cdr.pos*)))

(def one (item)
  (ret v (next)
    (unless ((testify item) v) (fail))))

(test-match (one #\a)
  "abc" #\a
  "xyz" '<<parse-fail>>)

(test-match (one alphadig)
  "abc" '#\a
  "#bc" '<<parse-fail>>)

(test-match (one (fn (x) t))
  "" '<<parse-fail>>)

(match "abcd"
  (test-iso (at (one letter)) #\a)
  (test-iso pos* '(#\a #\b #\c #\d)))

(test-match (do (next)
                (try x (n-of 2 (one letter))
                  'didnt-expect-success
                  pos*))
  "ab123" '(#\b #\1 #\2 #\3))

; todo: confusing name

(def onenot (item)
  (ret v (next)
    (unless ((complement (testify item)) v) (fail))))

(def oneof items
  (ret v (next)
    (unless (some [(testify _) v] items) (fail))))

(def falt2 (f1 f2)
  (let mark pos*
    (onfail
     (do (= pos* mark)
         (f2))
     (f1))))

(mac alt2 (p1 p2)
  `(falt2 (fn () ,p1) (fn () ,p2)))

(mac alt ps
  (if (no ps)
       `(fail)
      (no (cdr ps))
       (car ps)
       `(alt2 ,(car ps) (alt ,@(cdr ps)))))

(test-match (alt) "greetings" '<<parse-fail>>)

(mac optional body
  `(alt (do ,@body) nil))

(def mliteral1 (pat val)
  (let pat (ascons pat)
    (if (begins pos* pat)
         (do (= pos* (nthcdr len.pat pos*))
             val)
         (fail))))

(def mliteral args
  (if (no args)
       (fail)
      (no (cdr args))
       (mliteral1 (car args) (car args))
       (alt (mliteral1 (car args) (cadr args))
            (apply mliteral (cddr args)))))

(test-match (mliteral "xyz")
  "greetings" '<<parse-fail>>)
(test-match (mliteral "greet")
  "greetings" "greet")
(test-match (list (mliteral "greet")
                  (mliteral "ings"))
  "greetings" '("greet" "ings"))
(test-match (list (mliteral "greetx") (mliteral "ings"))
  "greetings" '<<parse-fail>>)

(test-match  (list (mliteral "greet") (mliteral "xings"))
  "greetings" '<<parse-fail>>)

(test-match (alt (mliteral "greet")) "greeting" "greet")

(test-match (alt (mliteral "greet")
                 (mliteral "foo"))
  "greetings" "greet")

(test-match (alt (mliteral "foo")
                 (mliteral "greet"))
 "greetings" "greet")

(test-match (alt (mliteral "foo")
                 (mliteral "bar")
                 (mliteral "greet"))
 "greetings" "greet")

(test-match (list (mliteral "greet")
                  (optional (mliteral "ings")))
 "greetings" '("greet" "ings"))

(test-match (list (mliteral "greet")
                  (optional (mliteral "xyz")))
 "greetings" '("greet" nil))

(def fmany (fp)
  (accum a
    (xloop ()
      (try v (fp)
        (do (a v)
            (next))
        nil))))

(mac many body
  `(fmany (fn () ,@body)))

(test-match (many (mliteral "x"))
 "xxxyz" '("x" "x" "x"))

(test-match (list (many (mliteral "x"))
                  (mliteral "y"))
  "xxxyz"   '(("x" "x" "x") "y")
  "yzzz"    '(nil "y")
  "xxxyzzz" '(("x" "x" "x") "y"))

(def manyis (v)
  (many (one v)))

(def manyisnt (v)
  (let test (complement (testify v))
    (many (one test))))

(mac many1 parser
  `(cons (do ,@parser) (many (do ,@parser))))

(test-match (list (many1 (mliteral "x"))
                  (mliteral "y"))
  "yzzz" '<<parse-fail>>)

;; todo better names for these
(def many1is (v)
  (let test (testify v)
    (many1 (one test))))

(mac must (errmsg . body)
  `(onfail (err ,errmsg)
     ,@body))

(test-match (must "want cookie!" (mliteral "cookie"))
  "xyzzy" (an-err "want cookie!"))

(mac parse-intersperse (separator parser must-message)
  `(optional
    (cons ,parser
          (many ,separator
                (must ,must-message ,parser)))))

(test-match (parse-intersperse
             (one #\,)
             (one alphadig)
             "comma must be followed by alphadig")
  "a,b,c" '(#\a #\b #\c))

(def skipwhite ()
  (manyis whitec))

(test-match (do (skipwhite)
                (one #\a))
  "  abc" #\a)

(mac comma-separated (parser (o must-message "a comma must be followed by a value"))
  `(parse-intersperse
    (do (skipwhite) (one #\,))
    ,parser
    ,must-message))

(test-match (comma-separated
             (one #\a)
             "comma must be followed by 'a'")
  "a  ,b  ,c" (an-err "comma must be followed by 'a'"))
 

(test-match (comma-separated
             (one alphadig)
             "comma must be followed by alphadig")
  "a  ,b  ,c" '(#\a #\b #\c))

(mac entire body
  `(do1 (do ,@body)
        (unless (at-end) (fail))))

(test-match (entire (mliteral "abc")) "abc" "abc")
(test-match (entire (mliteral "ab"))  "abc" '<<parse-fail>>)

(def fmatched-input (parser)
  (let mark pos*
    (parser)
    (accum a
      (xloop (p mark)
        (when (and p (isnt p pos*))
          (a (car p))
          (next (cdr p)))))))

(mac matched-input body
  `(fmatched-input (fn () ,@body)))

(test-match (matched-input (one #\a) (one #\b) (one #\c))
  "abcd" '(#\a #\b #\c))

(mac not body
  (w/uniq v
    `(try ,v (do ,@body)
       (fail)
       t)))

(test-match (do (mliteral "ab")
                (not (one #\!))
                (mliteral "c"))
  "abc" "c")

(test-match (do (mliteral "ab")
                (not (one #\!)))
  "ab"   t
  "ab!c" '<<parse-fail>>)

(def fupto (n parser)
  (if (< n 1)
       '()
      (try v (parser)
        (cons v (fupto (- n 1) parser))
        nil)))

(mac upto (n . body)
  `(fupto ,n (fn () ,@body)))

(def ffrom1upto (n parser)
  (or (upto n (parser))
      (fail)))

(mac from1upto (n . body)
  `(ffrom1upto ,n (fn () ,@body)))

(test-match (upto 3 (one #\a))
  "b"     '()
  "ab"    '(#\a)
  "aaab"  '(#\a #\a #\a)
  "aaaab" '(#\a #\a #\a))

(test-match (from1upto 3 (one #\a))
  ""      '<<parse-fail>>
  "b"     '<<parse-fail>>
  "ab"    '(#\a)
  "aaab"  '(#\a #\a #\a)
  "aaaab" '(#\a #\a #\a))
