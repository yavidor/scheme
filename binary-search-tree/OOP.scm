(define with (lambda (l f) (apply f l)))

(define concat
  (letrec
    ((c-two (lambda (l1 l2)
             (if (null? l1)
               l2
               (with l1 (lambda (a . s)
                         (cons a (c-two s l2)))))))
      (c-all (lambda (first rest)
              (if (null? rest)
                first
                (with rest (lambda (second . remaining) (c-all (c-two first second) remaining)))))))
    (lambda (a . s)
      (c-all a s))))

(define ^ctor
  (lambda (t fn a)
    (letrec ((f
               (cond
                 ((eq? fn 'get-val) get-val)
                 ((eq? fn 'get-left) get-left)
                 ((eq? fn 'get-right) get-right)
                 ((eq? fn 'empty?) empty?)
                 ((eq? fn 'leaf?) leaf?)
                 ((eq? fn 'pre-order) pre-order)
                 ((eq? fn 'in-order) in-order)
                 ((eq? fn 'post-order) post-order)
                 ((eq? fn 'add-val) add-val)
                 ((eq? fn 'search) search)
                 ((eq? fn 'find-min) find-min)
                 ((eq? fn 'find-max) find-max)
                 ((eq? fn 'remove-val) remove-val)
                 ((eq? fn 'remove-all-val) remove-all-val)
                 ((eq? fn 'height) height)
                 ((eq? fn 'balanced?) balanced?)
                 ((eq? fn 'balance) balance)
                 (else (display 'oops) (display fn) (newline) (exit 1)))))
      (if (null? a)
        (f t)
        (apply f t a)))))

(define ^node
  (lambda (v l r)
    (letrec
      ((f
          (lambda (fn . a)
            (if (procedure? fn)
              (fn v l r #f)
              (^ctor f fn a))))) ; For some reason it got messy with what variables it could see and what it couldn't, the idea is to get it back to the ^node function to keep it within the same closure
      f)))

;; (^empty): constructor for empty tree
(define ^empty
  (lambda ()
    (letrec
      ((f
          (lambda (fn . a)
            (if (procedure? fn)
              (fn 'bla 'bla 'bla #t)
              (^ctor f fn a)))))
      f)))

;; (^leaf v): constructor for node holding v with two empty subtrees
(define ^leaf
  (lambda (v) (^node v (^empty) (^empty))))

;; (get-val t): value stored at node t
(define get-val
  (lambda (t)
    (t (lambda (v l r n) v))))

;; (get-left t): left subtree of node t
(define get-left
  (lambda (t)
    (t (lambda (v l r n) l))))

;; (get-right t): right subtree of node t
(define get-right
  (lambda (t)
    (t (lambda (v l r n) r))))

;; (empty? t): #t if t is an empty tree (no values), #f otherwise
(define empty?
  (lambda (t)
    (t (lambda (v l r n) n))))

;; (leaf? t): #t if t is a node with no children, #f otherwise (including empty tree)
(define leaf?
  (lambda (t)
    (if (t 'empty?)
      #f
      (and ((t 'get-left) 'empty?) ((t 'get-right) 'empty?)))))

;; (pre-order t): list of values in node, left, right order
(define pre-order
  (letrec
    ((p-o (lambda (t)
           (if (t 'empty?)
             '()
             (concat (list (t 'get-val)) (p-o (t 'get-left)) (p-o (t 'get-right)))))))
    p-o))

;; (in-order t): list of values in left, node, right order (sorted ascending)
(define in-order
  (letrec
    ((i-o (lambda (t)
           (if (t 'empty?)
             '()
             (concat (i-o (t 'get-left)) (list (t 'get-val)) (i-o (t 'get-right)))))))
    i-o))

;; (post-order t): list of values in left, right, node order
(define post-order
  (letrec
    ((p-o
        (lambda (t)
          (if (t 'empty?)
            '()
            (concat (p-o (t 'get-left)) (p-o (t 'get-right)) (list (t 'get-val)))))))
    p-o))

; (add-val t n): new tree with n inserted; duplicates go to the right subtree
(define add-val
  (letrec
    ((a-v
        (lambda (t n)
          (display n)
          (if (t 'empty?)
            (^leaf n)
            (let
              ((v (t 'get-val))
                (l (t 'get-left))
                (r (t 'get-right)))
              (if (> v n)
                (^node v (a-v l n) r)
                (^node v l (a-v r n))))))))
    a-v))

;; (search t n): #t if n is in t, #f otherwise
(define search
  (letrec
    ((s
        (lambda (t n)
          (if (t 'empty?)
            #f
            (let
              ((v (t 'get-val))
                (l (t 'get-left))
                (r (t 'get-right)))
              (if (= v n)
                #t
                (if (< v n)
                  (s r n)
                  (s l n))))))))
    s))

;; (find-min t): smallest value in t (leftmost node); assumes t is non-empty
(define find-min
  (letrec
    ((f-m
        (lambda (t)
          (if ((t 'get-left) 'empty?)
            (t 'get-val)
            (f-m (t 'get-left))))))
    f-m))

;; (find-max t): largest value in t (rightmost node); assumes t is non-empty
(define find-max
  (letrec
    ((f-m
        (lambda (t)
          (if ((t 'get-right) 'empty?)
            (t 'get-val)
            (f-m (t 'get-right))))))
    f-m))

;; (remove-val t n): new tree with first value matching n removed; unchanged if n doesn't exist in t
;; if the removed node has a right sub-tree, have the right sub-tree's minimum take its place
;; otherwise have the left sub-tree take its place
(define remove-val
  (letrec
    ((r-v
        (lambda (t n)
          (let
            ((v (t 'get-val))
              (l (t 'get-left))
              (r (t 'get-right)))
            (if (t 'empty?)
              t
              (if (= v n)
                (if (r 'empty?)
                  l
                  (let
                    ((r-m (r 'find-min)))
                    (^node r-m l (r-v r r-m))))
                (if (> v n)
                  (^node v (r-v l n) r)
                  (^node v l (r-v r n)))))))))
    r-v))

;; (remove-all-val t n): new tree with all values matching n removed, if any such exist
(define remove-all-val
  (letrec
    ((r-a-v
        (lambda (t n)
          (let
            ((v (t 'get-val))
              (l (t 'get-left))
              (r (t 'get-right)))
            (if (t 'empty?)
              t
              (if (= v n)
                (if (r 'empty?)
                  (r-a-v l n)
                  (let
                    ((r-m (r 'find-min)))
                    (r-a-v (^node r-m l (remove-val r r-m)) n)))
                (if (> v n)
                  (^node v (r-a-v l n) r)
                  (^node v l (r-a-v r n)))))))))
    r-a-v))

;; (height t): number of nodes on the longest root-to-leaf path; (height (^empty)): 0
(define height
  (lambda (t)
    (letrec
      ((h
          (lambda (t n)
            (if (t 'empty?)
              n
              (max (h (t 'get-left) (+ 1 n)) (h (t 'get-right) (+ 1 n)))))))
      (h t 0))))

;; (balanced? t): #t if for every node, the difference between its subtrees' heights is at most 1
(define balanced?
  (lambda (t)
    (letrec
      ((b? (lambda (node)
            (if (node 'empty?)
              (cons #t 1)
              (let*
                ((l (node 'get-left))
                  (r (node 'get-right))
                  (l-result (b? l))
                  (r-result (b? r)))
                (if (and (car l-result) (car r-result) (<= (abs (- (cdr l-result) (cdr r-result))) 1))
                  (cons #t (+ (max (cdr l-result) (cdr r-result)) 1))
                  (cons #f (+ (cdr l-result) (cdr r-result)))))))))
      (car (b? t)))))

;; (balance t): returns a tree t* for which (and (balanced? t*) (equal? (in-order t) (in-order t*))) is #t,
;; and t* has the smallest possible height for its number of values
(define balance
  (letrec
    ((s (lambda (l n) ;split
          (if (= n 0)
            (cons '() l)
            (let
              ((r (s (cdr l) (- n 1))))
              (cons (cons (car l) (car r)) (cdr r))))))

      (l-n
        (lambda (l)
          (if (null? l)
            0
            (+ 1 (l-n (cdr l))))))

      (l-t-b-t ;list-to-balanced-tree
        (lambda (l)
          (if (null? l)
            (^empty)
            (let*
              ((m (quotient (l-n l) 2))
                (parts (s l m))
                (first (car parts))
                (second (cdr parts)))
              (^node (car second)
                (l-t-b-t first)
                (l-t-b-t (cdr second)))))))
      (b
        (lambda (t)
          (l-t-b-t (in-order t)))))
    b))
