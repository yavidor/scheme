(define with (lambda (l f) (apply f l)))
; 1. change the name of this file to Sequences.scm
; 2. complete these definitions so they match their documentations, the interface is already set but the implementation is stubbed
; 3. you can check your work by running `scheme SequencesTests.scm`

; empty sequence: '()
; non-empty sequence: (cons v f) where v is the first value and f is a stub function which generates the rest of the sequence

; (from n): returns a sequence with start-value n and continues in increments of 1
(define from
  (letrec
    ((f (lambda (n)
          (cons n (lambda () (from (+ n 1)))))))
    f))

; (range b e s): sequence b, b+s, b+2s, ... stopping before the first value past e,
; where past means > e for a positive s and < e for a negative s; s = 0 never passes e
; (e may be -inf.0 or +inf.0)
(define range
  (letrec
    ((r
        (lambda (b e s)
          (if (or (and (> s 0) (> b e)) (and (< s 0) (< b e)))
            '()
            (cons b (lambda () (r (+ b s) e s)))))))
    r))

; (take s n): returns a list of the first n elements of sequence s
(define take
  (letrec
    ((t
        (lambda (s n)
          (if (or (zero? n) (null? s))
            '()
            (cons (car s) (t ((cdr s)) (- n 1)))))))
    t))

; (list->seq l): returns a sequence iterating the elements of list l in order of appearance
(define list->seq
  (letrec
    ((l-t-s
        (lambda (l)
          (if (null? l)
            l
            (with l (lambda (a . s)
                     (cons a (lambda () (l-t-s s)))))))))
    l-t-s))

; (mapq s f): returns a sequence of the elements of s mapped with function f
(define mapq
  (letrec
    ((mq
        (lambda (s f)
          (if (null? s)
            s
            (cons (f (car s)) (lambda () (mapq ((cdr s)) f)))))))
    mq))

; (filterq s p): returns a sequence of the elements of s filtered with predicate p
(define filterq
  (letrec
    ((fq
        (lambda (s p)
          (if (null? s)
            s
            (if (p (car s))
              (cons (car s) (lambda () (fq ((cdr s)) p)))
              (fq ((cdr s)) p))))))
    fq))

; (foldlq f i s): returns a left-folded result of the elements of s with initial value i and binary operation f (assumes finite sequence)
;; returns f(...f(f(i, x_0), x_1), ... , x_n)
(define foldlq
  (letrec
    ((flq
        (lambda (f i s)
          (if(null? s)
            i
            (flq f (f i (car s)) ((cdr s)))))))
    flq))

; (foldrq f s i): returns a right-folded result of the elements of s with initial value i and binary operation f (assumes finite sequence)
;; returns f(x_0, f(x_1, ... f(x_n, i)))
(define foldrq
  (letrec
    ((frq
        (lambda (f s i)
          (if (null? s)
            i
            (f (car s) (frq f ((cdr s)) i))))))
    frq))

; (concatq s1 s2): returns a sequence concatenating the elements of s2 after the elements of s1
(define concatq
  (letrec
    ((cq
        (lambda (s1 s2)
          (if (null? s2)
            s1
            (if (null? s1)
              (cons (car s2) (lambda () (cq s1 ((cdr s2)))))
              (cons (car s1) (lambda () (cq ((cdr s1)) s2))))))))
    cq))

; (addq s1 s2): returns a sequence adding s1 and s2 element wise, if one sequence is shorter than the other it is viewed as padded with zeroes to match the length
(define addq
  (letrec
    ((aq
        (lambda (s1 s2)
          (if (and (null? s1) (null? s2))
            s1
            (if (null? s1)
              (cons (car s2) (lambda () (aq s1 ((cdr s2)))))
              (if(null? s2)
                (cons (car s1) (lambda () (aq ((cdr s1)) s2)))
                (cons (+ (car s1) (car s2)) (lambda() (aq ((cdr s1)) ((cdr s2)))))))))))

    aq))

; (interleaveq s1 s2): returns a sequence of the elements of s1 and s2 interleaved (elements of s1 at even indices and elements of s2 at odd indices), if one sequence ends, continue with the other one
(define interleaveq
  (letrec
    ((iq
        (lambda (s1 s2)
          (if (and (null? s1) (null? s2))
            s1
            (if (null? s1)
              (cons (car s2) (lambda () (iq s1 ((cdr s2)))))
              (if (null? s2)
                (cons (car s1) (lambda () (iq ((cdr s1)) s2)))
                (cons (car s1) (lambda () (iq s2 ((cdr s1))))))))))) ; Switch the positions of the first and second sequences to get the zipper pattern we want
    iq))

; (dropq s n): returns a sequence without first n elements of s
(define dropq
  (letrec
    ((dq
        (lambda (s n)
          (if (or (null? s) (zero? n))
            s
            (dq ((cdr s)) (- n 1)))))) ; The opposite of take
    dq))
