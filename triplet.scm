(define cons-three
  (lambda (a b c)
    (lambda (d)
      (d a b c #f))))

(define nil-three
  (lambda (c)
    (c 'bla 'bla 'bla #t)))

(define first (lambda (p)
               (p (lambda (a b c n) a))))

(define second (lambda (p)
                (p (lambda (a b c n) b))))

(define third (lambda (p)
               (p (lambda (a b c n) c))))

(define my-null? (lambda (p)
                  (p (lambda (a b c n) n))))

(define my-null? (lambda (p)
                  (eq? nil-three p)))

(define print-triplet (lambda (p)
                       (display #\()
                       (display (first p))
                       (display (quote #\,))
                       (display (second p))
                       (display (quote #\,))
                       (display (third p))
                       (display #\))))
