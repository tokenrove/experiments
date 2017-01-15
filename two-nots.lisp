

(progn
  (format t "~&a b c  P C  A B C  ok?~%------ - -  - - -  ---")
  (loop for i below 8
        for a = (ldb (byte 1 2) i)
        for b = (ldb (byte 1 1) i)
        for c = (ldb (byte 1 0) i)
        for carry = (logior (logand a c)
                            (logand a b)
                            (logand b c))
        for not-carry = (lognot carry)
        for parity = (lognot (logior (logand carry a b c)
                                     (logand not-carry (logior a b c))))
        for not-c = (logior (logand carry a b parity)
                            (logand not-carry (logior a b parity)))
        for not-a = (logior (logand carry b c parity)
                            (logand not-carry (logior b c parity)))
        for not-b = (logior (logand carry a c parity)
                            (logand not-carry (logior a c parity)))
        do (format t "~&~B ~B ~B  ~B ~B  ~B ~B ~B  ~A"
                   a b c (logand 1 parity) carry
                   (logand 1 not-a)
                   (logand 1 not-b)
                   (logand 1 not-c)
                   (if (and (= (logand 1 not-a) (logand 1 (lognot a)))
                            (= (logand 1 not-b) (logand 1 (lognot b)))
                            (= (logand 1 not-c) (logand 1 (lognot c))))
                       "+" "-"))))
