(define with (lambda (l f) (apply f l)))
; Weird how I cannot find an easier way to do it, the only other idea was to do cons with a reversed list and then reverse
(define append-to-list
  (letrec
    ((atl (lambda (l v)
      (if (null? l)
        (list v)
        (with l (lambda (a . s)
          (cons a (atl s v))))))))
    atl))

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
  (lambda (v) (list v (^empty) (^empty))))

;; (get-val t): value stored at node t
(define get-val 
  (lambda (t) (car t)))

;; (get-left t): left subtree of node t
(define get-left 
  (lambda (t) (car (cdr t))))

;; (get-right t): right subtree of node t
(define get-right 
  (lambda (t) (car (cdr (cdr t))))) ;Since cdr returns a pair we need to extract it, therefore the last car

;; (empty? t): #t if t is an empty tree (no values), #f otherwise 
(define empty? 
  (lambda (t) (null? (car t))))

;; (leaf? t): #t if t is a node with no children, #f otherwise (including empty tree)
(define leaf?
  (lambda (t) (or (empty? t) (and (empty? (get-left t)) (empty? (get-right t))))))

;; (pre-order t): list of values in node, left, right order
(define pre-order
  (lambda (t) 
    (if (empty? t)
      '()
      (concat (list (get-val t)) (pre-order (get-left t)) (pre-order (get-right t))))))

;; (in-order t): list of values in left, node, right order (sorted ascending)
(define in-order
  (lambda (t)
    (if (empty? t)
      '()
      (concat (in-order (get-left t)) (list (get-val t)) (in-order (get-right t))))))

;; (post-order t): list of values in left, right, node order
(define post-order
  (lambda (t)
  (if (empty? t)
    '()
    (concat (post-order (get-left t)) (post-order (get-right t)) (list (get-val t))))))

; (add-val t n): new tree with n inserted; duplicates go to the right subtree
(define add-val
  (lambda (t n)
    (let
      ((v (get-val t))
      (l (get-left t))
      (r (get-right t)))
        (if (empty? t)
          (^leaf n)
          (if (leaf? t)
            (if (> v n)
              (^node v (^leaf n) (^empty))
              (^node v (^empty) (^leaf n)))
            (if (> v n)
              (^node v (add-val l n) r)
              (^node v l (add-val r n))))))))

;; (search t n): #t if n is in t, #f otherwise
(define search
  (lambda (t n) '()))

;; (find-min t): smallest value in t (leftmost node); assumes t is non-empty
(define find-min
  (lambda (t) '()))

;; (find-max t): largest value in t (rightmost node); assumes t is non-empty
(define find-max
  (lambda (t) '()))

;; (remove-val t n): new tree with first value matching n removed; unchanged if n doesn't exist in t
;; if the removed node has a right sub-tree, have the right sub-tree's minimum take its place
;; otherwise have the left sub-tree take its place
(define remove-val
  (lambda (t n)
    (let
      ((v (get-val t))
      (l (get-left t))
      (r (get-right t)))
        (if (empty? t)
          '()
          (if (= v n)
            (^node (get-val r) l (get-right r))
            (if (> v n)
              (^node v (remove-val l n) r)
              (^node v l (remove-val r n))))))))

;; (remove-all-val t n): new tree with all values matching n removed, if any such exist
(define remove-all-val
  (lambda (t n)
    (let
      ((v (get-val t))
      (l (get-left t))
      (r (get-right t)))
        (if (empty? t)
          t
        (if (= v n)
          (^node (get-val r) (remove-all-val l n) (remove-all-val (get-right r) n))
          (^node v (remove-all-val l n) (remove-all-val r n)))))))

;; (height t): number of nodes on the longest root-to-leaf path; (height (^empty)): 0
(define height
  (lambda (t) (lambda (t n) '()) t 0)

;; (balanced? t): #t if for every node, the difference between its subtrees' heights is at most 1
(define balanced?
  (lambda (t) #f))

;; (balance t): returns a tree t* for which (and (balanced? t*) (equal? (in-order t) (in-order t*))) is #t,
;; and t* has the smallest possible height for its number of values
(define balance
  (lambda (t) '()))

