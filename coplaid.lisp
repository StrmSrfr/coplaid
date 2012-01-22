;;;; coplaid.lisp

(in-package #:coplaid)

(defparameter/ps *rows* 20)
(defparameter/ps *columns* 20)

; life-like
#+(or)
(defun/ps next-state (sum-of-neighbors)
  (cond
    ((< sum-of-neighbors 2)
     0)
    ((> sum-of-neighbors 3)
     0)
    (t
     1)))

(defun/ps next-state (cell sum-of-neighbors)
  (case sum-of-neighbors
    ((2) (if (= cell 1) 1 0))
    ((3) 1)
    (t 0)))

(defun/ps get-state (array x y)
  (if (or (minusp x) (minusp y))
      0
      (let*((row
	     (or (nth x array)
		 (make-list *columns* :initial-element 0)))
	    (cell
	     (or (nth y row)
		 0)))
	cell)))

(defun/ps sum-neighbors (array x y)
  (loop for i from (1- x) to (1+ x)
     sum (loop for j from (1- y) to (1+ y)
               unless (= i j)
		  sum (get-state array i j))))

(defun/ps compute-next (array)
  (loop for x from 0 to *rows*
    collect (loop for y from 0 to *columns*
      collect (next-state (get-state array x y)
			  (sum-neighbors array x y)))))

(defparameter/ps *glider*
    '((0 1 0)
      (0 0 1)
      (1 1 1)))

(defun asciify (array)
  (loop for y from 0 to *columns*
    do (loop for x from 0 to *rows*
	    do (princ (case (get-state array x y)
			((0) ".")
			((1) "#"))))
    do (terpri)))

(hunchentoot:define-easy-handler (viewer :uri "/coplaid/") (index)
  (setf (hunchentoot:content-type*) "text/html")
  (cl-who:with-html-output-to-string (s)
    (:html
     (:head
      (:title "coplaid: a javascript compute server")
      (:script :type "text/javascript"
               :src "//ajax.googleapis.com/ajax/libs/jquery/1.6.0/jquery.min.js")
      (:script :type "text/javascript"
               :src "/mum/ps-lisp-library.js")
      (:script :type "text/javascript"
	       (cl-who:str **rows**)
	       (cl-who:str **columns**)
	       (cl-who:str **glider**)
	       (cl-who:str *next-state*)
	       (cl-who:str *get-state*)
	       (cl-who:str *sum-neighbors*)
	       (cl-who:str *compute-next*))
      (:style :type "text/css"
       (cl-who:str
        (css-lite:css
	  ((".zero") (:background-color "black"))
	  ((".one") (:background-color "white")))))
      (:script :type "text/javascript"
        (cl-who:str
          (ps
	    (defun first (list)
	      (@ list 0))
	    (defun make-list (size &key initial-element)
	      (loop repeat size
		   collect initial-element))
	    (defun minusp (real)
	      (< real 0))
	    (defun nth (n list)
	      (if (< (length list) n)
		  nil
		  (elt list n)))
	    (defun plusp (real)
	      (> real 0))
	    (defun rest (array) ; TODO CHEAP
	      (ps:chain array (slice 1)))
            (defun some (predicate &rest sequences)
	      (loop do
		 (cond
		   ((null sequences)
		    (return nil))
		   ((not (reduce (lambda (a b) (and a b)) sequences))
		    (return nil))
		   ((apply predicate (mapcar #'first sequences))
		    (apply predicate (mapcar #'first sequences)))
		   (t
		    (setf sequences (mapcar #'rest sequences)))))))))

      (:script :type "text/javascript"
        (cl-who:str
          (ps
	    (defvar *state* *glider*)
	    (defun update-screen ()
	      (loop for x from 0 to *columns*
		   do (loop for y from 0 to *rows*
			   do
			   (chain ($ (+ "#cell_" x "_" y))
				  (remove-class)
				  (add-class (if (= 0 (get-state *state* x y))
						 "zero"
						 "one"))))))
	    (set-interval
	     (lambda ()
	       ;(loop while (some (lambda (row)
				;(some #'plusp row))
				;*state*)
		 (update-screen)
		 (setf *state* (compute-next *state*))) 500))))
      )
     (:body
      (:table :width "100%" :height "100%" :id "cells"
       (:tbody
	(loop for y from 0 to *rows*
	     do (cl-who:htm (:tr
		      (loop for x from 0 to *columns*
			 do (cl-who:htm (:td :class "zero"
					     :id (format nil "cell_~A_~A" y x)))))))))))))




