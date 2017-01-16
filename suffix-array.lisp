;;; Research to base final implementations on:
;;; - Puglisi and Turpin -- Space-Time Tradeoffs for Longest-Common-Prefix Array Computation
;;; - Nong, Zhang, and Chan -- Linear time suffix array construction using D-critical substrings

;;; Suffix array operations:
;;; - find all repetitions, by length descending

(defpackage #:suffix-array
  (:use :cl :ccl))
(in-package :suffix-array)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :fiveam)
  (require :alexandria)
  (require :ieee-floats)
  (use-package '(:fiveam)))

(defun simplest-possible-saca (string)
  "Naïve suffix array construction algorithm."
  (let* ((n (length string))
         (SA (make-array n :element-type 'fixnum)))
    (dotimes (i n) (setf (aref SA i) i))
    (sort SA (lambda (a b) (string< string string :start1 a :start2 b)))))

(defun sa-term-frequency (SA string term)
  (let ((n (length term))
        a b)
    (dotimes (i (length SA) (values a b (- b a)))
      (let ((j (aref SA i)))
        (when (string= string term :start1 j :end1 (min (length SA) (+ j n)))
          (if a (setf b i) (setf a i)))))))

(defun lcp-of-suffixes (s a b)
  (loop for n from (- (length s) (max a b)) downto 1
        if (string= s s :start1 a :start2 b :end1 (+ a n) :end2 (+ b n)) return n
        finally (return 0)))

(defun longest-common-prefixes (string sa)
  (let ((lcp (make-array (+ 1 (length string)) :element-type 'fixnum :initial-element -1)))
    (loop for j from 1 below (length sa)
          do (setf (aref lcp j) (lcp-of-suffixes string (aref sa (1- j)) (aref sa j))))
    lcp))

;; linear-time LCP
(defun get-height (s sa)
  (let ((rank (make-array (length sa) :element-type 'fixnum))
        (height (make-array (1+ (length sa)) :element-type 'fixnum :initial-element -1)))
    (dotimes (i (length sa)) (setf (aref rank (aref sa i)) i))
    (let ((h 0))
      (dotimes (i (length sa))
        (when (plusp (aref rank i))
          (let ((k (aref sa (1- (aref rank i)))))
            (do () ((not (eql (aref s (+ i h))
                              (when (< (+ k h) (length s)) (aref s (+ k h))))))
              (incf h))
            (setf (aref height (aref rank i)) h)
            (when (plusp h) (decf h))))))
    height))

;; XXX need to write tests for this
(defun non-left-extensible (i j bwt)
  (let ((l (aref bwt i)))
    (incf i)
    (loop while (and (<= i j) (eql l (aref bwt i))) do (incf i))
    (<= i j)))

(defparameter *minimum-period* 2)

(defun psy-1 (n lcp bwt)
  (let ((left-boundary '((-1 . -1)))
        (output nil)
        (i -1) (j -1) (p -1) (q 0) (prev-ne -1))
    (do () ((>= j n) output)
      (loop
        (setf j (1+ j) p q q (aref lcp (1+ j)))
        (when (and (> q p) (>= q *minimum-period*))
          (push (cons j q) left-boundary))
        (when (> p q) (return)))
      (loop
        (when (>= (cdar left-boundary) 0)
          (destructuring-bind (a . b) (pop left-boundary)
            (setf i a p b)
            (cond ((>= prev-ne i)
                   (push (list p i j) output))
                  ((non-left-extensible (1- i) (1- j) bwt)
                   (setf prev-ne i)
                   (push (list p i j) output)))))
        (when (<= (cdar left-boundary) q) (return)))
      (when (and (< (cdar left-boundary) q)
                 (>= q *minimum-period*))
        (push (cons i q) left-boundary)))))

;;(define-modify-macro minf (&rest args) min)

(defun simple-max-repeats (sa lcp)
  "Result is an array where maxreps[j] == i such that S[i..j] is a
maximum repeat."
  ;; XXX does not behave correctly if there are singletons (characters
  ;; that occur only once)
  (let* ((n (length sa))
         (max-reps (make-array n :element-type 'fixnum :initial-element (1+ n))))
    (dotimes (i n)
      (let ((a (1- (max (aref lcp i) (aref lcp (1+ i))))))
        (alexandria:minf (aref max-reps (+ (aref sa i) a)) (aref sa i))))
    max-reps))

(defun patterns-from-max-repeats (s)
  (let* ((sa (simplest-possible-saca s))
         (lcp (get-height s sa))
         (max-reps (simple-max-repeats sa lcp))
         (patterns (make-hash-table :test 'equal))
         (patlist ())
         (counter 0)
         (n (length s)))
    (flet ((add-pattern (i j)
             (let ((u (subseq s i j)))
               (push (cons (or (gethash u patterns)
                               (setf (gethash u patterns) (incf counter)))
                           u)
                     patlist))))
      (do ((i 0))
          ((>= i n))
        (let* ((j (position-if (lambda (x) (< x n)) max-reps :start i))
               (k (aref max-reps j)))
          (format t "~&~A ~A" i k)
          (when (> k i) (add-pattern i k))
          (add-pattern k (1+ j))
          (setf i (1+ j)))))
    ;; (values patterns patlist)
    (reverse patlist)))


(defun psy-2 (s)
  (let* ((sa (simplest-possible-saca s))
         (lcp (get-height s sa))
         (bwt (burrows-wheeler-transform s sa))
         (n (length s)))
    (flet ((snle (start end)
             (let ((k (1+ (- end start))))
               (when (< k 256)
                 (loop for h from start to end
                       with b = 0
                       do (let ((l (if (aref bwt h) (char-code (aref bwt h)) 257)))
                            (if (marked? b l)
                                (return-from snle nil)
                                (setf b (mark-range b l 1)))))
                 t))))
      (do ((j 0) (p -1) (q 0) high start)
          ((>= j (1- n)))
        (setf high 0)
        (loop
          (setf j (1+ j)
                p q
                q (aref lcp (1+ j)))
          (when (> q p) (setf high q start j))
          (when (> p q) (return)))
        (when (and (> high 0) (snle start j))
          (format t "~&output ~A; ~A" p (mapcar (lambda (i) (aref sa i)) (alexandria:iota (1+ (- j start)) :start start))))))))

(defun burrows-wheeler-transform (string sa)
  "BWT, with NIL as the sentinel element."
  (map-into (make-array (1+ (length sa)))
            (lambda (j) (when (plusp j) (aref string (1- j)))) sa))

(fiveam:test saca-1
  (let ((string "to be or not to be"))
    (is (every #'= #(15 2 8 5 12 16 3 17 4 9 14 1 6 10 7 11 13 0) (simplest-possible-saca string))))
  (let* ((string "ababababab")
         (sa (simplest-possible-saca string)))
    (is (every #'= #(8 6 4 2 0 9 7 5 3 1) sa))
    (is (every #'= #(-1 2 4 6 8 0 1 3 5 7 -1) (longest-common-prefixes string sa))))
  (let* ((string "abaababa")
         (sa (simplest-possible-saca string)))
    (is (every #'= #(7 2 5 0 3 6 1 4) sa))
    (is (every #'= #(-1 1 1 3 3 0 2 2 -1) (longest-common-prefixes string sa)))))


(defun make-range-set () 0)
;; (defun mark-range (S start end)
;;   (logior S (logxor (1- (ash 1 end)) (1- (ash 1 start)))))
;; (defun marked? (S pos) (= 1 (logand S (ash 1 pos))))
(defun mark-range (S start len) (dpb -1 (byte len start) S))
(defun marked? (S pos) (= 1 (ldb (byte 1 pos) S)))


(defun left (i) (1+ (ash i 1)))
(defun right (i) (+ 2 (ash i 1)))
(defun parent (i) (ash i -1))

(defun make-heap (A &key (test #'>=) (key #'identity))
  (let ((s (1- (length A))))
    (loop for i from (ash s -1) downto 0
          do (heapify A s i :test test :key key))
    (values A s)))

(defun heapify (A s i &key (test #'>=) (key #'identity))
  (let* ((l (left i)) (r (right i))
         (largest (if (and (<= l s)
                           (funcall test (funcall key (aref A l)) (funcall key (aref A i))))
                      l i))
         (largest (if (and (<= r s)
                           (funcall test (funcall key (aref A r)) (funcall key (aref A largest))))
                      r largest)))
    (when (/= largest i)
      (rotatef (aref A i) (aref A largest))
      (heapify A s largest :test test :key key))))

(defun heap-extract-top (A s)
  (prog1 (aref A 0)
    (setf (aref A 0) (aref A s))
    (decf s)
    (heapify A s 0)))

(fiveam:test heap-1
  (is (every #'= #(16 14 10 8 7 9 3 2 4 1) (make-heap #(4 1 3 2 16 9 10 14 8 7)))))


(defun cps-1 (s)
  (let* ((sa (simplest-possible-saca s))
         (lcp (get-height s sa))
         (pos (make-array (length s) :element-type 'fixnum :initial-element -1))
         (len (make-array (length s) :element-type 'fixnum :initial-element -1))
         stack)
    (do ((i -1) (j 0) (k 1))
        ((>= k (length s)))
      (loop while (<= (aref lcp j) (aref lcp k))
            do (progn (push i stack)
                      (setf i j
                            j k
                            k (1+ k))))
      (let ((p (aref sa i))
            (q (aref sa j))
            (l (aref lcp j)))
        (when (< p q) (rotatef p q))
        (setf (aref pos p) q (aref len p) l)
        (loop while (= (aref lcp i) l)
              do (progn
                   (setf i (pop stack))
                   (setf p (aref sa i))
                   (when (< p q) (rotatef p q))
                   (setf (aref pos p) q (aref len p) l)))
        (setf (aref sa i) q)
        (if (> i -1)
            (setf j i
                  i (pop stack))
            (setf j k
                  k (1+ k)))))
    (values pos len)))

(defun loop-packer (s)
  (let* ((SA (simplest-possible-saca s))
         (LCP (get-height s SA))
         (ranges (make-range-set))
         (size-limit 8))                ;XXX low so we can find bugs
    (loop for (lcp . i) across (sort (map 'vector #'cons LCP (alexandria:iota (length LCP))) #'> :key #'car)
          if (and (> lcp *minimum-period*)
                  (< lcp size-limit)
                  (not (marked? ranges (aref SA i)))
                  (< (abs (- (aref SA i) (aref SA (1- i)))) size-limit))
          do (progn
               (format t "~&got a loop at ~A: (~A,~A)" (max (aref SA i) (aref SA (1- i)))
                       (abs (- (aref SA i) (aref SA (1- i)))) lcp)
               (setf ranges (mark-range ranges (aref SA i) lcp))))))

(defun pattern-packer (s)
  (let* ((SA (simplest-possible-saca s))
         (LCP (get-height s SA))
         (ranges (make-range-set)))
    (loop for (lcp . i) across (sort (map 'vector #'cons LCP (alexandria:iota (length LCP))) #'> :key #'car)
          ;; shorten overlapping prefixes
          when (> lcp *minimum-period*) do (alexandria:minf lcp (abs (- (aref SA i) (aref SA (1- i)))))
          if (and (> lcp *minimum-period*)
                  (not (marked? ranges (aref SA i))))
          do (let ((n 0))
               ;; XXX find matches by moving forward then back
               (flet ((f (j)
                        ;;(format t "~&found a match at ~A (~A) from ~A" j (aref SA j) i)
                        (setf ranges (mark-range ranges (aref SA j) lcp))
                        (incf n))
                      (prefix= (i j)
                        (string= s s :start1 (aref SA i) :start2 (aref SA j) :end1 (+ (aref SA i) lcp) :end2 (min (+ (aref SA j) lcp) (length s)))))
                 (f i)
                 (loop for j from (1- i) downto 0
                       while (prefix= i j)
                       when (not (marked? ranges (aref SA j))) do (f j))
                 (loop for j from (1+ i) below (length SA)
                       while (prefix= i j)
                       when (not (marked? ranges (aref SA j))) do (f j))
                 (format t "~&Using ~A as a pattern (~A), found ~A times" (subseq s (aref SA i) (+ (aref SA i) lcp)) lcp n)
                 )))
    ranges))

;; Implementation of SACA per Kärkkäinen and Sanders

(defun radix-pass (a b r n K)
  (let ((c (make-array (1+ K) :element-type 'fixnum :initial-element 0))
        (sum 0))
    (dotimes (i n) (incf (aref c (aref r (aref a i)))))
    (dotimes (i (1+ K))
      (let ((u (aref c i)))
        (setf (aref c i) sum)
        (incf sum u)))
    (dotimes (i n)
      (let ((u (aref c (aref r (aref a i)))))
        (incf (aref c (aref r (aref a i))))
        (setf (aref b u) (aref a i))))))

(defun ks-saca (s k n &optional (SA (make-array n :element-type 'fixnum)))
  (let* ((n0 (floor (+ n 2) 3))
         (n1 (floor (+ n 1) 3))
         (n2 (floor n 3))
         (n02 (+ n0 n2))
         (s12 (make-array (+ 3 n02) :element-type 'fixnum :initial-element 0))
         (SA12 (make-array (+ 3 n02) :element-type 'fixnum :initial-element 0))
         (s0 (make-array n0 :element-type 'fixnum :initial-element 0))
         (SA0 (make-array n0 :element-type 'fixnum :initial-element 0)))
    (loop for i below (+ n (- n0 n1))
          with j = 0
          unless (= 0 (mod i 3)) do (progn (setf (aref s12 j) i)
                                           (incf j)))
    (radix-pass s12 SA12 (make-array (- (length s) 2) :displaced-to s :displaced-index-offset 2) n02 k)
    (radix-pass s12 SA12 (make-array (- (length s) 1) :displaced-to s :displaced-index-offset 1) n02 k)
    (radix-pass s12 SA12 s n02 k)

    (let ((name 0) (c0 -1) (c1 -1) (c2 -1))
      (dotimes (i n02)
        (when (or (/= c0 (aref s (aref SA12 i)))
                  (/= c1 (aref s (1+ (aref SA12 i))))
                  (/= c2 (aref s (+ 2 (aref SA12 i)))))
          (incf name)
          (setf c0 (aref s (aref SA12 i))
                c1 (aref s (1+ (aref SA12 i)))
                c2 (aref s (+ 2 (aref SA12 i)))))
        (setf (aref s12 (+ (floor (aref SA12 i) 3) (if (= 1 (mod (aref SA12 i) 3)) 0 n0)))
              name))

      (cond ((< name n02)
             (ks-saca s12 name n02 SA12)
             (dotimes (i n02) (setf (aref s12 (aref SA12 i)) (1+ i))))
            (t (dotimes (i n02) (setf (aref SA12 (1- (aref s12 i))) i)))))
    (let ((j 0))
      (dotimes (i n02)
        (when (< (aref SA12 i) n0)
          (setf (aref s0 j) (* 3 (aref SA12 i)))
          (incf j))))
    (radix-pass s0 SA0 s n0 K)

    (let ((p 0)
          (u (- n0 n1)))
      (dotimes (k n)
        (flet ((get-i () (if (< (aref SA12 u) n0) (1+ (* 3 (aref SA12 u))) (+ 2 (* 3 (- (aref SA12 u) n0))))))
          (let ((i (get-i))
                (j (aref SA0 p)))
            (cond ((if (< (aref SA12 u) n0)
                       (and (<= (aref s i) (aref s12 (+ (aref SA12 u) n0)))
                            (<= (aref s j) (aref s12 (floor j 3))))
                       (and (<= (aref s i) (aref s (1+ i)) (aref s12 (1+ (- (aref SA12 u) n0))))
                            (<= (aref s j) (aref s (1+ j)) (aref s12 (+ n0 (floor j 3))))))
                   (setf (aref SA k) i)
                   (incf u)
                   (when (= u n02)
                     (do ((p p (1+ p))
                          (k (1+ k) (1+ k)))
                         ((>= p n0))
                       (setf (aref SA k) (aref SA0 p)))))
                  (t
                   (setf (aref SA k) j)
                   (incf p)
                   (when (= p n02)
                     (do ((u u (1+ u))
                          (k (1+ k) (1+ k)))
                         ((>= u n0))
                       (setf (aref SA k) (get-i)))))))))))
  SA)
    
;;;; Radix sorts

(defun radix-sort-byte (v)
  (let ((histogram (make-array 256 :element-type 'fixnum)))
    (dovector (b v) (incf (aref histogram b)))
    (loop for i from 0
          for h across histogram
          with k = 0
          do (dotimes (j h)
               (setf (aref v k) i)
               (incf k))))
  v)

(require :ieee-floats)

(defun radix-sort-floats (v)
  (flet ((unfloat (f)
           (let ((i (ieee-floats:encode-float32 f)))
             (logxor (if (= 0 (ldb (byte 1 31) i)) #x80000000 #xFFFFFFFF) i)))
         (refloat (i)
           (ieee-floats:decode-float32 (logxor i (if (= 1 (ldb (byte 1 31) i)) #x80000000 #xFFFFFFFF)))))
    (let* ((buf1 (make-array (length v) :element-type '(unsigned-byte 32)))
           (buf2 (copy-seq buf1))
           (hn (ash 1 11))
           (histogram (make-array (* 3 hn) :element-type 'fixnum)))
      (dotimes (i (length v))
        (let* ((j (unfloat (aref v i))))
          (setf (aref buf1 i) j)
          (incf (aref histogram (ldb (byte 11 0) j)))
          (incf (aref histogram (+ hn (ldb (byte 11 11) j))))
          (incf (aref histogram (+ hn hn (ldb (byte 11 22) j))))))
      (let ((tsum 0) (sum0 0) (sum1 0) (sum2 0))
        (dotimes (i hn)
          (setf tsum (+ (aref histogram i) sum0)
                (aref histogram i) (1- sum0)
                sum0 tsum
                tsum (+ (aref histogram (+ i hn)) sum1)
                (aref histogram (+ i hn)) (1- sum1)
                sum1 tsum
                tsum (+ (aref histogram (+ i hn hn)) sum2)
                (aref histogram (+ i hn hn)) (1- sum2)
                sum2 tsum)))
      (dovector (i buf1)
        (let ((pos (ldb (byte 11 0) i)))
          (setf (aref buf2 (incf (aref histogram pos))) i)))
      (dovector (i buf2)
        (let ((pos (ldb (byte 11 11) i)))
          (setf (aref buf1 (incf (aref histogram (+ hn pos)))) i)))
      (dovector (i buf1)
        (let ((pos (ldb (byte 11 22) i)))
          (setf (aref buf2 (incf (aref histogram (+ hn hn pos)))) i)))
      (map 'vector #'refloat buf2))))


;;; Both FIBONACCI-STRING and THUE-MORSE-STRING could be implemented
;;; much more efficiently, but they only exist here for testing
;;; purposes.

(defun fibonacci-string (n)
  (case n
    (0 "")
    (1 "b")
    (2 "a")
    (otherwise (concatenate 'string (fibonacci-string (1- n)) (fibonacci-string (- n 2))))))

(defun population-count (b)
  (declare (type (unsigned-byte 32) b))
  (let* ((b (+ (logand b #x55555555) (logand (ash b -1) #x55555555)))
         (b (+ (logand b #x33333333) (logand (ash b -2) #x33333333)))
         (b (+ (logand b #x0f0f0f0f) (logand (ash b -4) #x0f0f0f0f)))
         (b (+ (logand b #x00ff00ff) (logand (ash b -8) #x00ff00ff))))
    (+ (logand b #xffff) (logand (ash b -16) #xffff))))

(defun thue-morse-string (n)
  (let ((A (make-array (ash 1 n) :element-type 'base-char)))
    (loop for i below (ash 1 n)
          do (setf (aref A i) (if (= 0 (logand (population-count i) 1)) #\a #\b)))
    A))
