(in-package #:coplaid)

(defun starify (symbol)
  "Intern a symbol is the same package as SYMBOL with stars around its name.
If the symbol already has stars, you get double stars!"
  (intern
   (concatenate 'string
		"*"
		(symbol-name symbol)
		"*")
   (symbol-package symbol)))

(defmacro defdef/ps (name thing)
  `(defmacro ,name (name &body body)
     (let ((form (list* ',thing name body)))
       `(progn
	  ,form
	  (defparameter ,(starify name)
	    (ps ,form))))))

(defdef/ps defun/ps defun)
(defdef/ps defparameter/ps defparameter)
(defmacro defparameter/ps (name &body body)
  `(progn
     (defparameter ,name ,@body)
     (defparameter ,(starify name)
       (ps (defvar ,name ,@body)))))
