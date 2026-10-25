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
          (if (> b e)
            '()
            (cons b (lambda () (r (+ b s) e s)))))))
    r))

; (take s n): returns a list of the first n elements of sequence s
(define take
  (letrec
    ((t
        (lambda (s n)
          (if (zero? n)
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
  (lambda (s p) '()))

; (foldlq f i s): returns a left-folded result of the elements of s with initial value i and binary operation f (assumes finite sequence)
;; returns f(...f(f(i, x_0), x_1), ... , x_n)
(define foldlq
  (lambda (f i s) '()))

; (foldrq f s i): returns a right-folded result of the elements of s with initial value i and binary operation f (assumes finite sequence)
;; returns f(x_0, f(x_1, ... f(x_n, i)))
(define foldrq
  (lambda (f s i) '()))

; (concatq s1 s2): returns a sequence concatenating the elements of s2 after the elements of s1
(define concatq
  (lambda (s1 s2) '()))

; (addq s1 s2): returns a sequence adding s1 and s2 element wise, if one sequence is shorter than the other it is viewed as padded with zeroes to match the length
(define addq
  (lambda (s1 s2) '()))

; (interleaveq s1 s2): returns a sequence of the elements of s1 and s2 interleaved (elements of s1 at even indices and elements of s2 at odd indices), if one sequence ends, continue with the other one
(define interleaveq
  (lambda (s1 s2) '()))

; (dropq s n): returns a sequence without first n elements of s
(define dropq
  (lambda (s n) '()))
