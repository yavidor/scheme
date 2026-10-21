(define map
  (letrec
    ((m (lambda (l f)
          (if (null? l)
            l
            (apply (lambda (a . s)
                    (cons (f a) (m s f)))
              l)))))
    m))
(map '(1 2 3 4 -5 6 -1 -2) abs)

(define reduce
  (letrec
    ((r (lambda (l reducer initial)
          (if (null? l)
            initial
            (apply (lambda (a . s)
                    (r s reducer (reducer initial a)))
              l)))))
    r))
(reduce '(2 2 3 4 -1) min 0)

(define concat
  (letrec
    ((c (lambda (l1 l2)
          (if (null? l1)
            l2
            (apply (lambda (a . s)
                    (cons a (c s l2)))
              l1)))))
    c))

(concat '(1 2 3) '(4 5 6))

(define reverse-list
  (letrec
    ((r (lambda (l)
          (if (null? l)
            '()
            (apply (lambda (a . s)
                    (concat (r s) (list a)))
              l)))))
    r))
(reverse-list '(1 2 3 4 5 6 7 8 9))
