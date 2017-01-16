
(defun dot-product (u v)
  (reduce #'+ (map 'vector #'* u v)))

(defun cost-fn (x)
  (dot-product x #(16 19 23 28)))

(defun constraint-fn (x)
  (>= 7 (dot-product x #(2 3 4 5))))

(defun value (i) (aref #(16 19 23 28) (1- i)))
(defun cost (i) (aref #(2 3 4 5) (1- i)))

(defun choose-item (A i j)
  (let ((nope (aref A (1- i) j))
        (yep (if (>= j (cost i)) (+ (value i) (aref A (1- i) (- j (cost i)))) 0)))
    (max nope yep)))

(defun extract-knapsack (A i j)
  (let ((v (make-array (list i) :element-type 'fixnum)))
    (loop for i from i downto 1
       do (let ((this (aref A i j))
                (left (aref A (1- i) j)))
            (when (< left this)
              (setf (aref v (1- i)) 1)
              (decf j (cost i)))))
    v))

(defun solve ()
  (let* ((n 4) (capacity 7)
         (A (make-array (list (1+ n) (1+ capacity)) :element-type 'fixnum)))
    (loop for i from 1 to n
       do (loop for j from 1 to capacity
             do (setf (aref A i j) (choose-item A i j))))
    (extract-knapsack A n capacity)))

