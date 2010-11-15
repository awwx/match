(= defined-variables* (table))

(def ac-defined-var? (name)
  (if defined-variables*.name scheme-t scheme-f))

(mac defvar (name impl)
  `(do (ac-set-global ',name ,impl)
       (set (defined-variables* ',name))
       nil))

(mac defvar-impl (name)
  (let gname (ac-global-name name)
    `(scheme ,gname)))

(mac undefvar (name)
  `(do (wipe (defined-variables* ',name))
       (ac-set-global ',name nil)))
