(define with (lambda (l f) (apply f l)))

(define delete
  (letrec
    ((d
        (lambda (l v)
          (if (null? l)
            l
            (with l (lambda (a . s)
                     (if (= v a)
                       s
                       (cons a (d s v)))))))))
    d))
(delete '(1 2 3 4 5 4 6) 4)

(define delete-all ;Delete all occurrences of v, not every element (:
  (letrec
    ((da
        (lambda (l v)
          (if (null? l)
            l
            (with l (lambda (a . s)
                     (if (= v a)
                       (da s v)
                       (cons a (da s v)))))))))
    da))
(delete-all '(1 2 3 4 5 4 6) 4)

(define foldr
  (letrec
    ((fr
        (lambda (l f i)
          (if (null? l)
            i
            (apply (lambda (a . s)
                    (f a (fr s f i)))
              l)))))
    fr))

(define foldl
  (letrec
    ((fl
        (lambda (l f i)
          (if (null? l)
            i
            (apply (lambda (a . s)
                    (fl s f (f a i)))
              l)))))
    fl))

(define exist
  (letrec
    ((e
        (lambda (l p)
          (if (null? l)
            #f
            (apply (lambda (a . s)
                    (or (p a) (e s p)))
              l)))))
    e))
; (exist (list 1 2 3 4 5 6 7 8 9) (lambda (v) (= v 2)))
; (exist (list 1 2 3 4 5 6 7 8 9) (lambda (v) (= v 10)))

(define all
  (letrec
    ((fa
        (lambda (l p)
          (if (null? l)
            #t
            (apply (lambda (a . s)
                    (and (p a) (fa s p)))
              l)))))
    fa))

(all (list 2 2 2 4 8 6 12 8 10) (lambda (v) (= (mod v 2) 0)))
(all (list 1 2 3 4 5 6 7 8 9) (lambda (v) (= (mod v 2) 0)))
