;;;
;;; Written by Matthieu Villeneuve in Imago.
;;;
;;; The original copyright notice from the file follows:
;;;
;;; IMAGO library
;;; General utilities
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(defun best-in-array (f array &key (test #'>))
  (let* ((best-index 0)
         (best-element (aref array 0))
         (best-score (funcall f best-element)))
    (loop for index from 1 to (length array)
          as element = (aref array index)
          as score = (funcall f element)
          when (funcall test score best-score)
          do (setf best-index index
                   best-element element
                   best-score score)
          finally (return (values best-element best-index best-score)))))

