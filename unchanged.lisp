;;;
;;; Various attempts at implementing a "do-until-unchanged" macro.
;;;
;;; Julian Squires / 2005.
;;;


;;;; SIMPLE-CHECKSUM version.

;;; It's too bad PSXHASH isn't available everywhere.

(defconstant +simple-hash-multiplier+ 31
  "Multiplication constant suggested by Kernighan and Pike for simple
hashing applications.")

(defun simple-checksum (x)
  "Provides an SXHASH-style checksum of X, but also sloppily handles
arrays and hash tables, like SBCL's PSXHASH.  However, this checksum
was only designed for use in comparing relatively similar structures,
so it shouldn't be used as a general replacement for something like
PSXHASH."
  (typecase x
    (array (simple-array-sum x))
    (hash-table (simple-hash-sum x))
    (t (sxhash x))))

(defun simple-hash-sum (hash-table)
  (loop for x being each hash-value of hash-table
	summing (simple-checksum x)))

(defun simple-array-sum (array)
  (loop for x across array
	summing (simple-checksum x)))

;;; Note: there are various ways we can make this much more efficient,
;;; but I don't think it really matters that much.
(defmacro do-until-unchanged1 (var &body body)
  "Loop BODY until VAR doesn't change (according to EQUALP) between
iterations."
  (let ((last-time (gensym)))
    `(let ((,last-time))
      (tagbody
       top
	 (setf ,last-time (simple-checksum ,var))
	 ,@body
	 (unless (equal ,last-time (simple-checksum ,var))
	   (go top)))
      ,var)))


;;;; Augment body version.

;;; find SETF, determine if it's modifying vars.


;;;; DO-UNTIL-UNCHANGED
;;; Change do-until-unchanged1 to whichever version is appropriate.

(defmacro do-until-unchanged (vars &body body)
  "Loop BODY until each variable in VARS doesn't change (according to
EQUALP) between iterations."
  (if vars
      `(do-until-unchanged1 ,(car vars)
	(do-until-unchanged ,(cdr vars)
	  ,@body))
      `(progn ,@body)))

