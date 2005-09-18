;;;
;;; Originally from my code in Imago for RLE during the PCX encoding
;;; stage.  I figure there are probably other useful things that can
;;; be done with it, especially with a bit of cleaning and
;;; generalization.
;;;
;;; Author: Julian Squires <julian@cipht.net> / 2005
;;;

(defmacro do-runs ((value index run accessor-fn length &key (test 'eql))
		   &body body)
  ;; XXX this docstring sucks.
  "Finds runs of a single value in a sequence of length LENGTH.  It
calls ACCESSOR-FN with an index into the sequence as the argument, and
executes the BODY form with the symbol passed as VALUE bound to the
value of the run, the symbol passed as INDEX bound to the index into
the sequence where the run starts, and the symbol passed as RUN bound
to the length of the run.  It is guaranteed to hit every value in the
sequence."
  (let ((access (gensym))
	(length-holder (gensym))
	(run-holder (gensym)))
    `(do* ((,length-holder ,length)
	   (,access ,accessor-fn)
	   (,index 0))
      ((>= ,index ,length-holder))
      (let* ((,value (funcall ,access ,index))
	     (,run-holder
	      (do ((j (1+ ,index) (1+ j)))
		  ((or (>= j ,length-holder)
		       (not (,test ,value (funcall ,access j))))
		   (- j ,index))))
	     (,run ,run-holder))
	,@body
	(incf ,index ,run-holder)))))
