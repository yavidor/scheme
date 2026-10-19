;for reduce testing
(define add
    (lambda (a b)
        (+ a b)))

(define min (lambda (a b) (if (> a b) b a)))

;For filter testing
(define greater-than-zero
    (lambda (n)
        (> n 0)))

; Length
(define list-length
        (lambda (l) 
            (if (null? l)
                    0
                    (+ 1 (list-length (cdr l))))))

(list-length '(1 2 3 4 5 6))
(list-length '(1 1 2 3 4 5 6))
(list-length '())


(define list-min
    (lambda (l) 
        (if (null? l)
            'EmptyList
            (if (null? (cdr l))
                (car l)
                (min (car l) (list-min (cdr l)))))))
(list-min '(1 2 3))
(list-min '())

;map
(define list-map
    (lambda (l func)
        (if (null? (cdr l))
            (cons (func (car l)) '())
            (cons (func (car l)) (list-map (cdr l) func)))))

(list-map (list 1 2 -1) abs)

;Reduce
(define reduce
    (lambda (l reducer initial)
        (if (null? (cdr l))
            (reducer initial (car l))
            (reduce (cdr l) reducer (reducer initial (car l))))))

(define my-list '(2 2 3 4))
(reduce my-list add 0)
(reduce my-list min (car my-list))

; Filter
(define filter
    (lambda (l predicate?)
        (if (null? (cdr l))
            (if (predicate? (car l))
                (cons (car l) '())
                '())
            (if (predicate? (car l))
                (cons (car l) (filter (cdr l) predicate?))
                (filter (cdr l) predicate?)))))

(filter (list 1 2 3 -1 -2 3 -3 -4 5) greater-than-zero)

;Concat
(define cat
  (lambda (l1 l2)
    (if (null? (cdr l1))
      (cons (car l1) l2)
      (cons (car l1) (cat (cdr l1) l2)))))

(cat my-list my-list)

;Reverse
(define reverse-list
  (lambda (l acc)
    (if (null? l)
       acc
      (reverse-list (cdr l) (cons (car l) acc)))))
(reverse-list my-list '())
