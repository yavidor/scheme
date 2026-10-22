(define with (lambda (l f) (apply f l)))

(define my-cons
  (lambda (a b)
    (lambda (c)
      (c a b #f))))

(define my-nil
  (lambda (c)
    (c 'bla 'bla #t)))

(define my-car (lambda (p)
                (p (lambda (a b n) a))))

(define my-cdr (lambda (p)
                (p (lambda (a b n) b))))

(define my-null? (lambda (p)
                  (p (lambda (a b n) n))))
