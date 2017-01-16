;;; Three sorting algorithms for lists of strings to implement:
;;; McIlroy, Bostic, McIlroy. Engineering Radix Sort.
;;; Andersson, Nilsson. Implementing Radixsort.
;;;

(defun subscripted-name-p (s)
  (and (symbolp s)
       (find #\_ (symbol-name s))))

(defun flatten (l)
  (if (consp l)
      (mapcan #'flatten l)
      (list l)))

(defmacro with-subscripted-arrays (&body body)
  ;; find variables with underscores in them
  ;; convert them to aref statements
  (let ((syms (remove-duplicates
               (remove-if-not #'subscripted-name-p (flatten body)))))
    `(progn
       (symbol-macrolet ,(mapcar (lambda (x) (let* ((s (symbol-name x))
                                               (i (position #\_ s)))
                                          `(,x (aref ,(read-from-string (make-array i :element-type 'character :displaced-to s))
                                                     ,(read-from-string (make-array (- (length s) (1+ i)) :element-type 'character :displaced-to s :displaced-index-offset (1+ i)))))))
                          syms)
         ,@body))))

(defun wegner-dutch-flag-partition (s)
  (labels ((f (s n depth)
             (with-subscripted-arrays
               (let* ((a (random n)) (b 1) (c (1- n)) (d (1- n)))
                 (rotatef s_a s_0)
                 (setf a 0)
                 ;; incf b til >
                 (loop with u = s_a
                       for v = s_b
                       while (>= u v)
                       do (when (= u v)
                            (incf a)
                            (rotatef s_a s_b))
                          (incf b))
                 ;; decf c til <
                 (loop with u = s_d
                       for v = s_c
                       while (>= u v)
                       do (when (= u v)
                            (decf d)
                            (rotatef s_c s_d))
                          (decf c))
                 ;; swap while b and c don't cross
                 (loop while (< b c)
                       do (rotatef s_b s_c)
                          (incf b)
                          (decf c))
                 ;; move a and d to center
                 (loop while (>= a 0)
                       do (rotatef s_b s_a)
                          (decf a)
                          (decf b))
                 (loop while (< d n)
                       do (rotatef s_c s_d)
                          (incf d)
                          (incf c))
                 (values s a b c d)))))
    (f s (length s) 0)))

(defun make-suffix-ptrs (s)
  (let* ((n (length s))
         (a (make-array n)))
    (prog1 a
      (dotimes (i n)
        (setf (aref a i)
              (make-array (- n i) :element-type 'character :displaced-to s :displaced-index-offset i))))))

(defun insertion-sort (s o n)
  (loop for i from 1 below n
        do (loop with item = (aref s (+ o i))
                 for j from (1- i) downto 0
                 while (string> (aref s (+ o j)) item)
                 do (setf (aref s (+ o (1+ j))) (aref s (+ o j)))
                 finally (setf (aref s (+ o (1+ j))) item)))
  s)

(defun simple-sort-1 (s n depth)
  (labels ((ch (x)
           (let ((u (aref s x)))
             (if (>= depth (length u)) 0
                 (char-code (char u depth)))))
         (swap (a b) (rotatef (aref s a) (aref s b)))
         (vs (i j n)
           (dotimes (k n)
             (swap i j) (incf i) (incf j))))
    (macrolet ((ptn (a b i)
                 `(loop while (<= b c)
                        do (case (signum (- u (ch ,b)))
                             (,i (return))
                             (0 (swap ,a ,b) (decf ,a ,i)))
                           (decf ,b ,i))))
      (when (<= n 1) (return-from simple-sort-1 s))
      (when (< n 15) (return-from simple-sort-1 (insertion-sort s n)))
      (swap (random n) 0)
      (let ((u (ch 0))
            (a 1) (b 1) (c (1- n)) (d (1- n)))
        (loop
          (ptn a b -1)
          (ptn d c 1)
          (unless (< b c) (return))
          (swap b c)
          (incf b)
          (decf c))
        (let ((u (min a (- b a))) (v (min (- d c) (- n d 1))))
          (vs 0 (- b u) u)
          (vs b (- n v) v))
        (let ((r (- b a)))
          (simple-sort-1 s r depth)
          (let ((m (+ a (- n d 1))))
            (when (not (zerop (ch r)))
              (simple-sort-1 (make-array m :displaced-to s
                                           :displaced-index-offset r)
                             m (1+ depth)))))
        (let ((r (- d c)))
          (simple-sort-1 (make-array r
                                     :displaced-to s
                                     :displaced-index-offset (- n r))
                         r depth))
        s))))

;;; Very fast for random data; much slower for highly regular, mostly
;;; sorted data.
(defun simple-sort-2 (s i n depth)
  (cond ((<= n 1) (return-from simple-sort-2 s))
        ((<= n 15) (return-from simple-sort-2 (insertion-sort s i n))))
  (labels ((ch (x)
           (let ((u (aref s (+ i x))))
             (if (>= depth (length u)) 0
                 (char-code (char u depth)))))
         (swap (a b) (rotatef (aref s (+ i a)) (aref s (+ i b))))
         (vs (i j n)
           (dotimes (k n) (swap i j) (incf i) (incf j))))
    (macrolet ((ptn (a b i)
                 `(loop while (<= b c)
                        do (case (signum (- u (ch ,b)))
                             (,i (return))
                             (0 (swap ,a ,b) (decf ,a ,i)))
                           (decf ,b ,i))))
      (swap (random n) 0)
      (let ((u (ch 0))
            (a 1) (b 1) (c (1- n)) (d (1- n)))
        (loop
          (ptn a b -1)
          (ptn d c 1)
          (unless (< b c) (return))
          (swap b c)
          (incf b)
          (decf c))
        (let ((u (min a (- b a))) (v (min (- d c) (- n d 1))))
          (vs 0 (- b u) u)
          (vs b (- n v) v))
        (let ((r (- b a)))
          (simple-sort-2 s i r depth)
          (let ((m (+ a (- n d 1))))
            (simple-sort-2 s (+ i r) m (1+ depth))))
        (let ((r (- d c))) (simple-sort-2 s (+ i (- n r)) r depth))
        s))))

(defun simple-sort (x) (simple-sort-2 x 0 (length x) 0))

(defun wegner-1 (x &key (n (length x)) (key #'identity))
  (labels ((swap (a b) (rotatef (aref x a) (aref x b)))
           (vs (i j n) (dotimes (k n) (swap i j) (incf i) (incf j))))
    (let ((a 1) (b 1) (c (1- n)) (d (1- n))
          (u (funcall key (aref x 0))))
      (loop
        (loop while (< b n)
              do (ecase (signum (- u (funcall key (aref x b))))
                   (-1 (return))
                   (0 (swap a b) (incf a))
                   (1))
                 (incf b))
        (loop while (> c 0)
              do (ecase (signum (- u (funcall key (aref x c))))
                   (-1)
                   (0 (swap c d) (decf d))
                   (1 (return)))
                 (decf c))
        (unless (< b c) (return))
        (swap b c)
        (incf b)
        (decf c))
      (let ((r (min a (- b a)))) (vs 0 (- b r) r))
      (let ((r (min (- d c) (- n d 1)))) (vs b (- n r) r))
      (values x b c))))
