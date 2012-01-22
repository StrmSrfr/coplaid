;;;; coplaid.asd

(asdf:defsystem #:coplaid
  :serial t
  :depends-on (#:alexandria
               #:cl-who
               #:hunchentoot
               #:eager-future2
               #:parenscript
               #:css-lite)
  :components ((:file "package")
               (:file "macros")
               (:file "coplaid")))

