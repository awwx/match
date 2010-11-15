; Test whether our reader parses something the same as Racket's
; reader.  (We may not always want to be the same, but it's good to at
; least know when we're different).

(mac test-parses-like-racket1 (parser input)
  `(test-match ,parser ,input ',(read input)))

(mac test-parses-like-racket (parser . inputs)
  `(do ,@(map (fn (example)
                `(test-parses-like-racket1 ,parser ,example))
              inputs)))

;; char

(def hexdigit (c)
  (or (<= #\a c #\f) (<= #\A c #\F) (<= #\0 c #\9)))

(def match-digit (base)
  (case base
    2  (oneof #\0 #\1)
    8  (oneof #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
    10 (one digit)
    16 (one hexdigit)
       (err "unknown base" base)))

(test-match (match-digit 2) "2" '<<parse-fail>>)
(test-match (match-digit 10)
  "9" #\9
  "A" '<<parse-fail>>)
(test-match (match-digit 16) "A" '#\A)

(def aschar (x)
  (coerce x 'char))

(def intchar (b s)
  (aschar (int (string s) b)))

(def backslash-octal ()
  (intchar 8 (from1upto 3 (match-digit 8))))

(test-match (backslash-octal)
  "09"    #\nul
  "41abc" #\!
  "101"   #\A)

(def backslash-hex2 ()
  (one #\x)
  (must "a \\x must be followed by one or two hex digits"
        (intchar 16 (from1upto 2 (match-digit 16)))))

(test-match (backslash-hex2) "x41" #\A)

(def backslash-hex4 ()
  (one #\u)
  (must "a \\u must be followed by one to four hex digits"
        (intchar 16 (from1upto 4 (match-digit 16)))))

(test-match (backslash-hex4) "u0041" #\A)

(def backslash-hex8 ()
  (one #\U)
  (must "a \\U must be followed by one to eight hex digits"
        (intchar 16 (from1upto 8 (match-digit 16)))))

(test-match (backslash-hex8) "U00000041" #\A)

(def named-char ()
  (mliteral
   "null"      #\nul
   "nul"       #\nul
   "backspace" #\backspace
   "tab"       #\tab
   "newline"   #\newline
   "linefeed"  #\newline
   "vtab"      #\vtab
   "page"      #\page
   "return"    #\return
   "space"     #\space
   "rubout"    #\rubout))

(test-match (named-char) "tab" #\tab)

(def char-constant ()
  (one #\#)
  (one #\\)
  (must "invalid character constant"
        (alt (do1 (named-char) (not (one letter)))
             (backslash-octal)
             (backslash-hex2)
             (backslash-hex4)
             (backslash-hex8)
             (do1 (next) (not (one letter))))))

(test-parses-like-racket (char-constant)
  "#\\null"
  "#\\space(a b c)"
  "#\\167"
  ;; "#\\x41" not implemented in PLT 4.2.1
  "#\\u0041"
  "#\\U00000041"
  "#\\λ"
  )

(test-match (char-constant)
  "#\\nulx" (an-err "invalid character constant"))


(mac defrule (name test . body)
  (let arglist (sig name)
    (w/uniq (orig args)
      `(let ,orig ,name
         (= ,name
            (fn ,args
              (aif (apply (fn ,arglist ,test) ,args)
                    (apply (fn ,arglist ,@body) ,args)
                    (apply ,orig ,args))))))))

(mac defalt (name . body)
  (if (bound name)
       (w/uniq orig
         `(let ,orig ,name
            (= ,name
               (fn ()
                 (alt (do ,@body)
                      (,orig))))))
       `(def ,name () ,@body)))


(defalt parse-value-here (char-constant))

(test-match (parse-value-here) "#\\a" #\a)
  

(def match-line-comment ()
  (one #\;)
  (manyisnt #\newline)
  (optional (one #\newline)))

(test-matchpos ";blah blah\nfoo" (match-line-comment) "foo")


(def match-block-comment ()
  (mliteral "#|")
  (many (alt (match-block-comment)
             (do (not (mliteral "|#"))
                 (next))))
  (must "no closing |#" (mliteral "|#")))

(test-matchpos "#| a |#foo"           (match-block-comment) "foo")
(test-matchpos "#| a #| b |# c |#foo" (match-block-comment) "foo")

(test-match (match-block-comment)
  "#| abc"                 (an-err "no closing |#")
  "#|#|#|#|abc|#|#|#xyzzy" (an-err "no closing |#"))


(def match-expression-comment ()
  (mliteral "#;")
  (manyis whitec)
  (parse-value-here))

(test-matchpos "#;#\\a 2"   (match-expression-comment) " 2")
(test-matchpos "#;  #\\a 2" (match-expression-comment) " 2")

(def match-comment ()
  (alt (match-line-comment)
       (match-block-comment)
       (match-expression-comment)))

(def skip-comments-and-whitespace ()
  (many (alt (one whitec)
             (match-comment))))

(test-matchpos "  ; foo\n  #|blah|# \n abc"
               (skip-comments-and-whitespace)
               "abc")

(def parse-value ()
  (skip-comments-and-whitespace)
  (parse-value-here))

(test-match (parse-value) "#;#\\a  #\\b" #\b)


;; string

(def match-string-backslash-char ()
  (case (next)
    #\a #\u0007
    #\b #\backspace
    #\t #\tab
    #\n #\newline
    #\v #\vtab
    #\f #\u000C
    #\r #\return
    #\e #\u001B
    #\" #\"
    #\' #\'
    #\\ #\\
        (fail)))

(test-match (match-string-backslash-char) "n" #\newline)

(def match-string-backslash-newline ()
  (optional (one #\return))
  (one #\newline))

(test-matchpos "\nfoo" (match-string-backslash-newline) "foo")

(def match-string-backslash-sequence ()
  (one #\\)
  (when (at-end)
    (err "a backslash in a string must be followed by a character"))
  (must "invalid backslash sequence in string"
    (alt (match-string-backslash-char)
         (backslash-octal)
         (backslash-hex2)
         (backslash-hex4)
         (backslash-hex8))))

(test-match (match-string-backslash-sequence) "\\u0041" #\A)

(def parse-string ()
  (string
   (accum a
     (one #\")
     (many (alt (a (match-string-backslash-sequence))
                (match-string-backslash-newline)
                (a (onenot #\"))))
     (must "missing closing quote in string"
           (one #\")))))

(test-parses-like-racket (parse-string)
  "\"\""
  "\"abc\""
  "\"\\n\""
  "\"a\\u41!\""
  "\"abc\\ndef\""
  )

(defalt parse-value-here (parse-string))

(test-match (parse-value)
  "#\\a"    #\a
  "\"abc\"" "abc")


;; quoted symbols like |abc| and \f\o\o

(def delimiter (c)
  (in c #\( #\) #\[ #\] #\{ #\} #\" #\, #\' #\` #\;))

(def terminator (c)
  (or (delimiter c) (whitec c)))

(def match-backslash-sym-char ()
  (one #\\)
  (must "backslash must be followed by a character"
        (next)))

(test-match (match-backslash-sym-char) "\\A" #\A)

(mac do2 (a b . cs)
  `(do ,a (do1 ,b ,@cs)))

(def match-bar-quote ()
  (do2 (one #\|)
       (manyisnt #\|)
       (must "missing closing |"
             (one #\|))))

(def begins-quoted-sym (acc)
  (alt (acc (match-backslash-sym-char))
       (map acc (match-bar-quote))))

(def in-quoted-sym (acc)
  (alt (acc (match-backslash-sym-char))
       (map acc (match-bar-quote))
       (acc (onenot terminator))))

(def charssym (cs)
  (sym (coerce cs 'string)))

(def parse-quoted-sym ()
  (charssym (accum a (begins-quoted-sym a)
                     (many (in-quoted-sym a)))))

(defalt parse-value-here (parse-quoted-sym))

(test-match (parse-value)
  "|a|"    'a
  "\\a\\b" 'ab)

(def this ()
  (coerce (many1is ~terminator) 'string))

(def parse-unquoted-sym-or-number ()
  (alt (do (at (one #\#)
               (oneof #\b #\B #\o #\O #\x #\X))
           (let this (this)
             (or (scheme.string->number this)
                 (err "invalid number" this))))

       (do (not (at (oneof #\# #\\)))
           (let this (this)
             (when (is this ".") (fail))
             (or (scheme.string->number this)
                 (coerce this 'sym))))))

(test-match (parse-unquoted-sym-or-number)
  "123"   123
  "1+"    '1+
  "#x100" 256
  "#foo"  '<<parse-fail>>
  "."     '<<parse-fail>>)
       
(defalt parse-value-here (parse-unquoted-sym-or-number))

(test-parses-like-racket (parse-value)
  "123"
  "123("
  "123abc"
  "123abc("
  "\\3"
  "#x3")


;; lists

; )

(def match-list-end ()
  (skip-comments-and-whitespace)
  (one #\)))

; . b

(def match-dotted-end ()
  (skip-comments-and-whitespace)
  (one #\.)
  (at (one terminator))
  (must "a dotted list period must be followed by a single value and then the closing parenthesis"
        (do1 (parse-value)
             (match-list-end))))

(test-match (match-dotted-end)
  ". x )"     'x
  ".;foo\nx)" 'x)

; able to produce dotted lists: (join2 '(a b 'c)) => (a b . c)

(def join2 (xs ys)
  (if (no xs)
       ys
       (cons (car xs) (join2 (cdr xs) ys))))

; can have (a b c d . e) or (a . b), but not (. a)

(def match-list-values ()
  (let xs (many1 (parse-value))
    (alt (join2 xs (match-dotted-end))
         (must "missing closing parenthesis"
               (match-list-end)
               xs))))

(def parse-list ()
  (one #\()
  (skip-comments-and-whitespace)
  (alt (do (one #\)) nil)
       (match-list-values)))

(test-match (parse-list)
  "()"        '()
  "(a b c)"   '(a b c)
  "(a b . c)" '(a b . c))

(defalt parse-value-here (parse-list))

(test-match (parse-value) "(a (b c (d)) e)" '(a (b c (d)) e))


(= quote-abbreviations* (pair '(
 "'"  quote
 "`"  quasiquote
 ",@" unquote-splicing  ; need to match ,@ before ,
 ","  unquote)))

(def parse-quote-abbreviation ((name expansion))
  (mliteral name)
  (skip-comments-and-whitespace)
       (must (string "a " name " must be followed by a value")
             `(,expansion ,(parse-value))))

(def parse-quote-abbreviations ((o abbrevs quote-abbreviations*))
  (if (no abbrevs)
       (fail)
       (alt (parse-quote-abbreviation (car abbrevs))
            (parse-quote-abbreviations (cdr abbrevs)))))

(test-match (parse-quote-abbreviations) "'a" ''a)

(defalt parse-value-here (parse-quote-abbreviations))

(test-parses-like-racket (parse-value) ",@foo")
 
(= brackets* (tuples '(
 "[" "]" square-bracket
 "{" "}" curly-bracket
) 3))
 
(def match-bracket ((open close expansion))
  (mliteral open)
  (let l (many (parse-value))
    (must (string open " without closing " close)
          (mliteral close)
          `(,expansion ,@l))))

(def match-brackets ((o brackets brackets*))
  (if (no brackets)
       (fail)
       (alt (match-bracket (car brackets))
            (match-brackets (cdr brackets)))))

(test-match (match-brackets) "[a b c]" '(square-bracket a b c))
