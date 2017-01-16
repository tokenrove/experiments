

(defun thread-nested (list)
  (reduce (lambda (accum next) (append accum (list next)))
	  (reverse list) :from-end t))

(defmacro → (&body body)
  ""
  (thread-nested body))

(defun delist (list)
  (destructuring-bind (a . b) list
    (if b
	(cons a (delist b))
	a)))