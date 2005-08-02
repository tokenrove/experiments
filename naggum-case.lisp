;;;
;;; Posted by Erik Naggum on comp.lang.lisp, 2001/09/04.
;;; Message-ID: <3208606982556119@naggum.net>
;;;

;;; Examples of use:

#|
 (with-hashed-identity (:test #'equal)
   (case "foo"
     ("foo" 'yeah)
     (t 'bummer)))

 (with-hashed-identity (:test #'equalp)
   (case "foo"
     ("FOO" 'yeah)
     (t 'bummer)))

 (with-hashed-identity (:test #'equalp)
   (case (vector #\f #\o #\o)
     (#(#\F #\O #\O) 'yeah)
     (t 'bummer)))
|#

;;;; Code.

(defun extract-case-keys (case-form)
(loop for clause in (cddr case-form)
   until (and (eq (car case-form) 'case) (member (car clause) '(t otherwise)))
   if (listp (car clause))
     append (car clause)
   else
     collect (car clause)))

;; export if packaged
(defparameter *with-hashed-identity-body-forms*
 '((case . extract-case-keys)
   (ccase . extract-case-keys)
   (ecase . extract-case-keys))
"Alist of the valid operators in body forms of a with-hashed-identity form
with their key-extraction function.")

(defun with-hashed-identity-error (body)
(error "Body form of with-hashed-identity is ~A, but must be one of:~{ ~A~}."
      (caar body) (mapcar #'car *with-hashed-identity-body-forms*)))

(defun with-hashed-identity-hashtable (hash-table body)
(dolist (key (funcall (or (cdr (assoc (car body) *with-hashed-identity-body-forms*))
                         'with-hashed-identity-error)
                     body))
 (setf (gethash key hash-table) key))
hash-table)

;; export if packaged
(defmacro with-hashed-identity (hash-options &body body)
"A wrapper around case forms to enable case tests via a hashtable."
(unless (and (listp (car body))
            (null (cdr body)))                 ;TODO: Allow multiple body forms.
 (error "Body of with-hashed-identity must be a single form.")
(let ((hash-table (make-symbol "hashtable")))
 `(let ((,hash-table (load-time-value
                      (with-hashed-identity-hashtable (make-hash-table ,@hash-options)
                        ',(car body)))))
    (,(caar body) (gethash ,(cadar body) ,hash-table) ,@(cddar body)))))

