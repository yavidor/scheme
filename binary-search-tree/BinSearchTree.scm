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

(define list-length
  (letrec
    ((l-n
        (lambda (l)
          (if (null? l)
            0
            (+ 1 (l-n (cdr l)))))))
    l-n))

; 1. change the name of this file to BinSearchTree.scm
; 2. complete these definitions so they match their documentations, the interface is already set but the implementation is stubbed
; 3. you can check your work by running `scheme BinSearchTreeTests.scm`

;; (^empty): constructor for empty tree
(define ^empty
  (lambda () (list '() '() '())))

;; (^node v l r): constructor for node holding v with left subtree l and right subtree r
(define ^node
  (lambda (v l r) (list v l r)))

;; (^leaf v): constructor for node holding v with two empty subtrees
(define ^leaf
  (lambda (v) (^node v (^empty) (^empty))))

;; (get-val t): value stored at node t
(define get-val car)

;; (get-left t): left subtree of node t
(define get-left cadr)

;; (get-right t): right subtree of node t
(define get-right caddr)

;; (empty? t): #t if t is an empty tree (no values), #f otherwise
(define empty?
  (lambda (t) (null? (car t))))

;; (leaf? t): #t if t is a node with no children, #f otherwise (including empty tree)
(define leaf?
  (lambda (t)
    (if (empty? t)
      #f
      (and (empty? (get-left t)) (empty? (get-right t))))))

;; (pre-order t): list of values in node, left, right order
(define pre-order
  (letrec
    ((p-o (lambda (t)
           (if (empty? t)
             '()
             (concat (list (get-val t)) (p-o (get-left t)) (p-o (get-right t)))))))
    p-o))

;; (in-order t): list of values in left, node, right order (sorted ascending)
(define in-order
  (letrec
    ((i-o (lambda (t)
           (if (empty? t)
             '()
             (concat (i-o (get-left t)) (list (get-val t)) (i-o (get-right t)))))))
    i-o))

;; (post-order t): list of values in left, right, node order
(define post-order
  (letrec
    ((p-o
        (lambda (t)
          (if (empty? t)
            '()
            (concat (p-o (get-left t)) (p-o (get-right t)) (list (get-val t)))))))
    p-o))

; (add-val t n): new tree with n inserted; duplicates go to the right subtree
(define add-val
  (letrec
    ((a-v
        (lambda (t n)
          (if (empty? t)
            (^leaf n)
            (let
              ((v (get-val t))
                (l (get-left t))
                (r (get-right t)))
              (if (> v n)
                (^node v (a-v l n) r)
                (^node v l (a-v r n))))))))
    a-v))

;; (search t n): #t if n is in t, #f otherwise
(define search
  (letrec
    ((s
        (lambda (t n)
          (if (empty? t)
            #f
            (let
              ((v (get-val t))
                (l (get-left t))
                (r (get-right t)))
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
          (if (empty? (get-left t))
            (get-val t)
            (f-m (get-left t))))))
    f-m))

;; (find-max t): largest value in t (rightmost node); assumes t is non-empty
(define find-max
  (letrec
    ((f-m
        (lambda (t)
          (if (empty? (get-right t))
            (get-val t)
            (f-m (get-right t))))))
    f-m))

;; (remove-val t n): new tree with first value matching n removed; unchanged if n doesn't exist in t
;; if the removed node has a right sub-tree, have the right sub-tree's minimum take its place
;; otherwise have the left sub-tree take its place
(define remove-val
  (letrec
    ((r-v
        (lambda (t n)
          (let
            ((v (get-val t))
              (l (get-left t))
              (r (get-right t)))
            (if (empty? t)
              t
              (if (= v n)
                (if (empty? r)
                  l
                  (let
                    ((r-m (find-min r)))
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
            ((v (get-val t))
              (l (get-left t))
              (r (get-right t)))
            (if (empty? t)
              t
              (if (= v n)
                (if (empty? r)
                  (r-a-v l n)
                  (let
                    ((r-m (find-min r)))
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
            (if (empty? t)
              n
              (max (h (get-left t) (+ 1 n)) (h (get-right t) (+ 1 n)))))))
      (h t 0))))

;; (balanced? t): #t if for every node, the difference between its subtrees' heights is at most 1
(define balanced?
  (lambda (t)
    (letrec
      ((b? (lambda (node)
            (if (empty? node)
              (cons #t 1)
              (let*
                ((l (get-left node))
                  (r (get-right node))
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

      (l-t-b-t ;list-to-balanced-tree
        (lambda (l)
          (if (null? l)
            (^empty)
            (let*
              ((m (quotient (list-length l) 2))
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
