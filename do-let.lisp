
(defmacro do-let1 (form where &rest bindings)
  (assert (equal where :where))
  `(let ,bindings
     ,form))

(do-let1 (+ a b)
  :where (a 10)
         (b 32))

(defmacro do-let (&body body-etc)
  (let* ((where-clause (position :where body-etc))
         (body (subseq body-etc 0 where-clause))
         (bindings (subseq body-etc (1+ where-clause))))
    `(let ,bindings
       ,@body)))

(do-let
  (format t "~&~A and ~A" a b)
  (+ a b)
  :where (a 10)
         (b 32))
