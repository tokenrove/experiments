
(defun histogram-of-bytes (data)
  (let ((histogram (make-array 256 :initial-element 0 :element-type 'fixnum))
        (count 0))
    (loop for x across data
          do (incf (aref histogram x))
             (incf count))
    (loop for y below 16
          do (loop for x below 16
                   do (format t "~<~4,1f~> " (float (* 100 (/ (aref histogram (+ x (* 16 y))) count)))))
             (format t "~%"))))

(defun histogram-of-file (path)
  (with-open-file (in path :element-type 'unsigned-byte)
    (let* ((n (file-length in))
           (A (make-array n :element-type 'unsigned-byte)))
      (read-sequence A in)
      (histogram-of-bytes A))))
