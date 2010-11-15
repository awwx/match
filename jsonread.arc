(defalt parse-json-value-here
  (mliteral
   "true"  t
   "false" nil
   "null"  nil))

(test-match (parse-json-value-here) "true" t)


(def parse-json-value ()
  (skipwhite)
  (parse-json-value-here))

(test-match (parse-json-value) "   true" t)


(def json-number-char (c)
  (find c ".-+eE1234567890"))

(def parse-json-number-here ()
  (coerce (string (many1is json-number-char)) 'num))

(test-match (parse-json-number-here)
  "1234" 1234
  "1e7"  1e7)

(defalt parse-json-value-here (parse-json-number-here))

(test-match (parse-json-value) "  45.6" 45.6)


(def parse-fourhex ()
  (must
   "four hex digits required after \\u"  
   (coerce (int (string (n-of 4 (one hexdigit))) 16) 'char)))

(test-match (parse-fourhex) "0041" #\u0041)

(def parse-json-backslash-char (c)
  (case c
    #\" #\"
    #\\ #\\
    #\/ #\/
    #\b #\backspace
    #\f #\page
    #\n #\newline
    #\r #\return
    #\t #\tab
    (err "invalid backslash char" c)))

(def parse-json-backslash-escape ()
  (one #\\)
  (alt (do (one #\u) (parse-fourhex))
       (parse-json-backslash-char:next)))

(test-match (parse-json-backslash-escape)     "\\n" #\newline)
(test-match (parse-json-backslash-escape) "\\u0041" #\A)

(def parse-json-string ()
  (one #\")
  (must "missing closing quote in JSON string"
    (do1 (string (many (alt (parse-json-backslash-escape)
                            (one [isnt _ #\"]))))
         (one #\"))))

(defalt parse-json-value-here (parse-json-string))

(test-match (parse-json-value)
  "\"\""        ""
  "\"abc\""     "abc"
  "\"ab\\ncd\"" "ab\ncd"
  "\"abc"       (an-err "missing closing quote in JSON string"))


(defalt parse-json-value-here
  (one #\[)
  (do1 (comma-separated (parse-json-value)
         "a comma in a JSON array must be followed by an object")
       (must "missing closing ] in JSON array"
             (skipwhite) (one #\]))))

(test-match (parse-json-value)
  "[]"          nil
  "[1,2,3]"     '(1 2 3)
  "[1,[2,3],4]" '(1 (2 3) 4)
  "[ 1 , 2  ]"  '(1 2)
  "[1,]"        (an-err "a comma in a JSON array must be followed by an object")
  "[1,2"        (an-err "missing closing ] in JSON array"))

(def parse-json-object-kv ()
  (let key (do (skipwhite) (parse-json-string))
    (must "a JSON object key string must be followed by a colon"
          (skipwhite) (one #\:))
    (let value (must "a colon in a JSON object must be followed by a value"
                     (parse-json-value))
      (list key value))))

(test-match (parse-json-object-kv) "\"a\":1" '("a" 1))

(def json-object ()
  (one #\{)
  (do1 (listtab
        (comma-separated
         (parse-json-object-kv)
         "a comma in a JSON object must be followed by a key"))
       (must "a JSON object must be terminated with a closing }"
             (skipwhite) (one #\}))))

(test-match (tablist (json-object)) "{\"a\":1}" '(("a" 1)))
