;; Tests for BinSearchTree.scm
;; run with: scheme --script BinSearchTreeTests.scm
;; exits with 0 if every test passes, 1 otherwise
;;
;; The tests only use the tree through its interface (^empty, ^node, ^leaf,
;; get-val, get-left, get-right, empty?), so they work however a tree is stored.
;; Trees in failure messages are written in this notation:
;;   _            the empty tree
;;   5            a node holding 5 with two empty subtrees
;;   5[3 _]       a node holding 5, left subtree 3, empty right subtree
;;   50[30[20 40] 70]

(load (let
       ((dir (path-parent (car (command-line)))))
       (if (string=? dir "")
         "BinSearchTree.scm"
         (string-append dir "/BinSearchTree.scm"))))

;;; ------------------------------------------------------------------
;;; test framework
;;; ------------------------------------------------------------------

;; how much work a single test may do before it counts as stuck in an infinite loop
;; (about 8 bytes of memory per tick if the loop is a runaway recursion)
(define FUEL 1000000)

(define passed 0)
(define failed 0)

;; (condition->string e): printable description of a raised condition
(define condition->string
  (lambda (e)
    (with-output-to-string
      (lambda ()
        (if (condition? e)
          (display-condition e)
          (write e))))))

;; (report-fail name detail): prints a failure and counts it
(define report-fail
  (lambda (name detail)
    (set! failed (+ failed 1))
    (display "  FAIL: ")
    (display name)
    (newline)
    (display "        ")
    (display detail)
    (newline)))

;; (run-limited thunk fuel): calls thunk, allowing it at most fuel ticks of work;
;; returns (ok . value), (error . message) or (timeout)
(define run-limited
  (lambda (thunk fuel)
    ((make-engine
        (lambda ()
          (guard (e (#t (cons 'error (condition->string e))))
            (cons 'ok (thunk)))))
      fuel
      (lambda (ticks-left result) result)
      (lambda (engine) (list 'timeout)))))

;; (run-test name expr thunk fuel): runs thunk, which returns (got . want) as
;; printed strings, and passes if they match; errors and infinite loops count as failures
(define run-test
  (lambda (name expr thunk fuel)
    (let
      ((r (run-limited thunk fuel)))
      (case (car r)
        ((ok) (if (equal? (cadr r) (cddr r))
               (set! passed (+ passed 1))
               (report-fail name (format "~s => ~a, expected ~a" expr (cadr r) (cddr r)))))
        ((error) (report-fail name (format "raised: ~a  in ~s" (cdr r) expr)))
        (else (report-fail name (format "did not finish (infinite loop?)  in ~s" expr)))))))

;; (test name expr expected): passes if expr evaluates to something equal? to expected
(define-syntax test
  (syntax-rules ()
    ((_ name expr expected)
      (run-test name 'expr
        (lambda () (cons (format "~s" expr) (format "~s" expected)))
        FUEL))))

;; (tree->string t): t in the notation described at the top of this file,
;; built only from empty?, get-val, get-left and get-right; gives up past
;; MAX-PRINT-DEPTH levels, which usually means empty? never returns #t
(define MAX-PRINT-DEPTH 500)

(define tree->string
  (letrec
    ((ts (lambda (t depth)
          (cond ((> depth MAX-PRINT-DEPTH)
                 (error #f (format "tree is over ~a levels deep while printing it; does empty? return #t for (^empty), and get-left / get-right return (^empty) for a leaf?"
                            MAX-PRINT-DEPTH)))
            ((empty? t) "_")
            ((and (empty? (get-left t)) (empty? (get-right t)))
              (format "~s" (get-val t)))
            (else (format "~s[~a ~a]"
                   (get-val t)
                   (ts (get-left t) (+ depth 1))
                   (ts (get-right t) (+ depth 1))))))))
    (lambda (t)
      (ts t 0))))

;; (test-tree name expr expected): passes if the tree expr evaluates to has the
;; shape and values written in expected, a string in tree->string notation
(define-syntax test-tree
  (syntax-rules ()
    ((_ name expr expected)
      (run-test name 'expr
        (lambda () (cons (tree->string expr) expected))
        FUEL))))

;; (test-true name expr) / (test-false name expr): shorthands for boolean results
(define-syntax test-true
  (syntax-rules ()
    ((_ name expr) (test name (if expr #t #f) #t))))

(define-syntax test-false
  (syntax-rules ()
    ((_ name expr) (test name (if expr #t #f) #f))))

;; (property name trials gen check): runs (check (gen)) trials times;
;; counts as one test, and on failure prints the first counterexample
(define property
  (lambda (name trials gen check)
    (let loop ((i 0))
      (if (= i trials)
        (set! passed (+ passed 1))
        (let*
          ((input (gen))
            (r (run-limited (lambda () (check input)) FUEL)))
          (case (car r)
            ((ok) (if (cdr r)
                   (loop (+ i 1))
                   (report-fail name (describe-case input))))
            ((error) (report-fail name (format "~a~%        raised: ~a" (describe-case input) (cdr r))))
            (else (report-fail name (format "~a~%        did not finish (infinite loop?)" (describe-case input))))))))))

;; (build what thunk fuel): value of thunk, for trees shared by several tests;
;; if it fails, prints a note and returns #f, so the tests using it fail on their own
(define build
  (lambda (what thunk fuel)
    (let
      ((r (run-limited thunk fuel)))
      (case (car r)
        ((ok) (cdr r))
        (else (display (format "  NOTE: could not build ~a (~a); tests using it will fail~%"
                        what
                        (if (eq? (car r) 'error) (cdr r) "did not finish")))
          #f)))))

;; (section title): prints a section header
(define section
  (lambda (title)
    (newline)
    (display "== ")
    (display title)
    (newline)))

;;; ------------------------------------------------------------------
;;; helpers (built only on the public interface)
;;; ------------------------------------------------------------------

;; (list->tree l): tree made by adding the values of l in order
(define list->tree
  (lambda (l)
    (fold-left add-val (^empty) l)))

;; (size t): number of values in t
(define size
  (lambda (t)
    (length (in-order t))))

;; (valid-bst? t): #t if every left-subtree value is <= the node and every
;; right-subtree value is >= it (duplicates may sit on either side after balance)
(define valid-bst?
  (letrec
    ((ok? (lambda (t lo hi)
           (or (empty? t)
             (let
               ((v (get-val t)))
               (and (or (not lo) (>= v lo))
                 (or (not hi) (<= v hi))
                 (ok? (get-left t) lo v)
                 (ok? (get-right t) v hi)))))))
    (lambda (t)
      (ok? t #f #f))))

;; (strict-bst? t): like valid-bst?, but left-subtree values must be < the node,
;; which is the shape add-val alone produces
(define strict-bst?
  (letrec
    ((ok? (lambda (t lo hi)
           (or (empty? t)
             (let
               ((v (get-val t)))
               (and (or (not lo) (>= v lo))
                 (or (not hi) (< v hi))
                 (ok? (get-left t) lo v)
                 (ok? (get-right t) v hi)))))))
    (lambda (t)
      (ok? t #f #f))))

(define sort-list
  (lambda (l)
    (list-sort < l)))

;; (remove-first x l): l without its first occurrence of x
(define remove-first
  (lambda (x l)
    (cond ((null? l) l)
      ((= x (car l)) (cdr l))
      (else (cons (car l) (remove-first x (cdr l)))))))

;; (remove-every x l): l without any occurrence of x
(define remove-every
  (lambda (x l)
    (filter (lambda (y) (not (= x y))) l)))

;; (min-height n): smallest possible height of a tree holding n values
(define min-height
  (lambda (n)
    (let loop ((h 0))
      (if (>= (- (expt 2 h) 1) n)
        h
        (loop (+ h 1))))))

;; (random-list max-len range): random-length list of values in [0, range)
(define random-list
  (lambda (max-len range)
    (let loop ((n (random (+ max-len 1))) (acc '()))
      (if (zero? n)
        acc
        (loop (- n 1) (cons (random range) acc))))))

;; (random-case): a random list of values (small range, so plenty of duplicates),
;; a value to look up, and whether to balance the tree first
(define random-case
  (lambda ()
    (list (random-list 25 12) (random 14) (zero? (random 2)))))

;; (random-unbalanced-case): like random-case, but never balanced first
(define random-unbalanced-case
  (lambda ()
    (let
      ((c (random-case)))
      (list (car c) (cadr c) #f))))

;; (describe-case c): readable description of a random case, for failure messages
(define describe-case
  (lambda (c)
    (format "fails for the tree made by adding ~s in this order~a, with value ~s"
      (car c)
      (if (caddr c) " and then calling balance" "")
      (cadr c))))

;; (case-tree c): the tree a random case describes
(define case-tree
  (lambda (c)
    (let
      ((t (list->tree (car c))))
      (if (caddr c) (balance t) t))))

(define TRIALS 1000)
(random-seed 20261016)

;; sample tree used throughout:
;;           50
;;         /    \
;;       30      70
;;      /  \    /  \
;;    20   40  60   80
(define sample-vals '(50 30 70 20 40 60 80))
(define sample (build "the sample tree with add-val" (lambda () (list->tree sample-vals)) FUEL))

;;; ------------------------------------------------------------------
;;; tests
;;; ------------------------------------------------------------------

(section "constructors and accessors")
(test-true "^empty is empty" (empty? (^empty)))
(test "^leaf value" (get-val (^leaf 5)) 5)
(test-true "^leaf left is empty" (empty? (get-left (^leaf 5))))
(test-true "^leaf right is empty" (empty? (get-right (^leaf 5))))
(test-tree "^leaf builds a single-value tree" (^leaf 5) "5")
(test-tree "^node with empty children is a leaf" (^node 5 (^empty) (^empty)) "5")
(test-tree "^node builds a three-value tree" (^node 2 (^leaf 1) (^leaf 3)) "2[1 3]")
(test "^node value" (get-val (^node 2 (^leaf 1) (^leaf 3))) 2)
(test-tree "^node left" (get-left (^node 2 (^leaf 1) (^leaf 3))) "1")
(test-tree "^node right" (get-right (^node 2 (^leaf 1) (^leaf 3))) "3")
(test-false "node is not empty?" (empty? (^leaf 1)))

(section "leaf?")
(test-true "leaf is leaf?" (leaf? (^leaf 1)))
(test-false "empty tree is not leaf?" (leaf? (^empty)))
(test-false "node with left child is not leaf?" (leaf? (^node 2 (^leaf 1) (^empty))))
(test-false "node with right child is not leaf?" (leaf? (^node 2 (^empty) (^leaf 3))))
(test-false "node with two children is not leaf?" (leaf? sample))

(section "traversals")
(test "pre-order of sample" (pre-order sample) '(50 30 20 40 70 60 80))
(test "in-order of sample" (in-order sample) '(20 30 40 50 60 70 80))
(test "post-order of sample" (post-order sample) '(20 40 30 60 80 70 50))
(test "pre-order of empty" (pre-order (^empty)) '())
(test "in-order of empty" (in-order (^empty)) '())
(test "post-order of empty" (post-order (^empty)) '())
(test "traversals of a leaf"
  (list (pre-order (^leaf 7)) (in-order (^leaf 7)) (post-order (^leaf 7)))
  '((7) (7) (7)))
(test "in-order of left-only chain" (in-order (list->tree '(3 2 1))) '(1 2 3))
(test "pre-order of right-only chain" (pre-order (list->tree '(1 2 3))) '(1 2 3))

(section "add-val")
(test-tree "add to empty gives leaf" (add-val (^empty) 5) "5")
(test-tree "smaller value goes left" (add-val (^leaf 5) 3) "5[3 _]")
(test-tree "larger value goes right" (add-val (^leaf 5) 8) "5[_ 8]")
(test-tree "equal value goes right" (add-val (^leaf 5) 5) "5[_ 5]")
(test-tree "builds expected shape" (list->tree '(2 1 3)) "2[1 3]")
(test-tree "sample has expected shape" sample "50[30[20 40] 70[60 80]]")
(test-tree "deeper insert lands in the right place"
  (add-val sample 35)
  "50[30[20 40[35 _]] 70[60 80]]")
(test "negative numbers" (in-order (list->tree '(0 -5 5 -10 10))) '(-10 -5 0 5 10))
(test "non-integer numbers" (in-order (list->tree '(1.5 0.25 2.5 1/2))) '(0.25 1/2 1.5 2.5))
(test-tree "add-val does not change the original tree"
  (begin (add-val sample 1) sample)
  "50[30[20 40] 70[60 80]]")

(section "search")
(for-each (lambda (v)
           (test-true (format "finds ~a in sample" v) (search sample v)))
  sample-vals)
(for-each (lambda (v)
           (test-false (format "does not find ~a in sample" v) (search sample v)))
  '(10 25 45 55 65 90 0 -50))
(test-false "nothing in empty tree" (search (^empty) 5))
(test-true "finds value in leaf" (search (^leaf 5) 5))
(test-false "missing value in leaf" (search (^leaf 5) 6))
(test-true "finds duplicated value" (search (list->tree '(5 5 5)) 5))

(section "find-min / find-max")
(test "min of sample" (find-min sample) 20)
(test "max of sample" (find-max sample) 80)
(test "min of leaf" (find-min (^leaf 4)) 4)
(test "max of leaf" (find-max (^leaf 4)) 4)
(test "min of right-only chain is root" (find-min (list->tree '(1 2 3))) 1)
(test "max of left-only chain is root" (find-max (list->tree '(3 2 1))) 3)
(test "min with negatives" (find-min (list->tree '(0 -7 3 -2))) -7)

(section "height")
(test "height of empty" (height (^empty)) 0)
(test "height of leaf" (height (^leaf 1)) 1)
(test "height of sample" (height sample) 3)
(test "height of chain" (height (list->tree '(1 2 3 4 5))) 5)
(test "height of lopsided tree" (height (list->tree '(10 5 15 3 1))) 4)

(section "remove-val")
(test-tree "remove from empty" (remove-val (^empty) 5) "_")
(test-tree "remove only value" (remove-val (^leaf 5) 5) "_")
(test-tree "remove missing value leaves tree unchanged" (remove-val sample 99) "50[30[20 40] 70[60 80]]")
(test "remove leaf"
  (in-order (remove-val sample 20))
  '(30 40 50 60 70 80))
(test-tree "remove node with only a left child" (remove-val (list->tree '(5 3 1)) 3) "5[1 _]")
(test-tree "remove node with only a right child" (remove-val (list->tree '(5 7 9)) 7) "5[_ 9]")
(test-tree "remove node with two children uses right subtree's minimum"
  (remove-val sample 30)
  "50[40[20 _] 70[60 80]]")
(test-tree "remove root" (remove-val sample 50) "60[30[20 40] 70[_ 80]]")
;; regression: the replacement must be the right subtree's minimum, not its root
(define regression-tree
  (build "(remove-val (list->tree '(50 30 70 60 80)) 50)"
    (lambda () (remove-val (list->tree '(50 30 70 60 80)) 50))
    FUEL))
(test "regression: root replaced by right minimum" (in-order regression-tree) '(30 60 70 80))
(test-true "regression: 60 still findable" (search regression-tree 60))
(test-true "regression: still a valid BST" (valid-bst? regression-tree))
(test "removes only one duplicate"
  (in-order (remove-val (list->tree '(5 5 5)) 5))
  '(5 5))
(test-tree "remove-val does not change the original tree"
  (begin (remove-val sample 50) sample)
  "50[30[20 40] 70[60 80]]")

(section "remove-all-val")
(test-tree "remove-all from empty" (remove-all-val (^empty) 5) "_")
(test-tree "remove-all missing value" (remove-all-val sample 99) "50[30[20 40] 70[60 80]]")
(test "remove-all single occurrence" (in-order (remove-all-val sample 50)) '(20 30 40 60 70 80))
(test "remove-all duplicates"
  (in-order (remove-all-val (list->tree '(50 30 70 50 60 50 80)) 50))
  '(30 60 70 80))
(test-tree "remove-all when every value matches" (remove-all-val (list->tree '(5 5 5 5)) 5) "_")
(test "remove-all keeps other duplicates"
  (in-order (remove-all-val (list->tree '(5 3 5 3 7 5)) 5))
  '(3 3 7))
;; regression: balance can put a duplicate in the left subtree
(test-tree "regression: remove-all on balanced duplicates"
  (remove-all-val (balance (list->tree '(5 5 5))) 5)
  "_")
(test "regression: remove-all on balanced tree with duplicates"
  (in-order (remove-all-val (balance (list->tree '(1 2 2 2 2 2 3))) 2))
  '(1 3))
(test-tree "remove-all-val does not change the original tree"
  (begin (remove-all-val sample 30) sample)
  "50[30[20 40] 70[60 80]]")

(section "balanced?")
(test-true "empty is balanced" (balanced? (^empty)))
(test-true "leaf is balanced" (balanced? (^leaf 1)))
(test-true "two nodes are balanced" (balanced? (list->tree '(5 3))))
(test-false "chain of three is not balanced" (balanced? (list->tree '(1 2 3))))
(test-true "full tree is balanced" (balanced? sample))
(test-true "height difference of one is balanced" (balanced? (list->tree '(4 2 6 1))))
(test-false "imbalance below a balanced root is detected"
  (balanced? (list->tree '(50 30 70 20 10 60 80 90))))
(test-false "equal heights at root but unbalanced children"
  (balanced? (list->tree '(50 30 70 20 80 10 90))))
(test "balanced? returns a boolean, not a height" (balanced? sample) #t)

(section "balance")
(test-tree "balance of empty" (balance (^empty)) "_")
(test-tree "balance of leaf" (balance (^leaf 1)) "1")
(test-tree "balance of chain of three" (balance (list->tree '(1 2 3))) "2[1 3]")
(define chain-100 (build "a 100-value chain with add-val" (lambda () (list->tree (iota 100))) FUEL))
(define balanced-100 (build "a balanced 100-value chain" (lambda () (balance chain-100)) FUEL))
(test "chain of 100 has height 100" (height chain-100) 100)
(test "balanced chain of 100 has height 7" (height balanced-100) 7)
(test-true "balanced chain is balanced?" (balanced? balanced-100))
(test "balance keeps in-order" (in-order balanced-100) (iota 100))
(test-true "balance gives a valid BST" (valid-bst? balanced-100))
(test "balance of already-balanced sample keeps values" (in-order (balance sample)) (in-order sample))
(test "balance is idempotent"
  (tree->string (balance (balance sample)))
  (tree->string (balance sample)))
(test-true "balance with duplicates is balanced?" (balanced? (balance (list->tree '(5 5 5 5 5)))))

(section "larger trees")
;; adding sorted values one by one is slow (each add walks the whole chain), so allow more work
(define chain-2000 (build "a 2000-value chain with add-val" (lambda () (list->tree (iota 2000))) (* 10 FUEL)))
(define balanced-2000 (build "a balanced 2000-value chain" (lambda () (balance chain-2000)) FUEL))
(test "2000-value chain in-order" (in-order chain-2000) (iota 2000))
(test "2000-value balanced height" (height balanced-2000) (min-height 2000))
(test-true "search deep in 2000-value chain" (search chain-2000 1999))
(test "remove-all over 2000-value balanced tree"
  (in-order (fold-left remove-all-val balanced-2000 (iota 1000)))
  (map (lambda (i) (+ i 1000)) (iota 1000)))

(section "properties on random trees (with duplicates, some balanced)")
(property "in-order is the sorted input" TRIALS random-case
  (lambda (c) (equal? (in-order (case-tree c)) (sort-list (car c)))))
(property "add-val trees are strict BSTs" TRIALS random-unbalanced-case
  (lambda (c) (strict-bst? (list->tree (car c)))))
(property "every tree is a valid BST" TRIALS random-case
  (lambda (c) (valid-bst? (case-tree c))))
(property "pre-, in- and post-order hold the same values" TRIALS random-case
  (lambda (c)
    (let
      ((t (case-tree c)))
      (and (equal? (sort-list (pre-order t)) (in-order t))
        (equal? (sort-list (post-order t)) (in-order t))))))
(property "first of pre-order and last of post-order is the root" TRIALS random-case
  (lambda (c)
    (let
      ((t (case-tree c)))
      (or (empty? t)
        (and (= (car (pre-order t)) (get-val t))
          (= (car (last-pair (post-order t))) (get-val t)))))))
(property "search agrees with list membership" TRIALS random-case
  (lambda (c)
    (eq? (if (search (case-tree c) (cadr c)) #t #f)
      (if (memv (cadr c) (car c)) #t #f))))
(property "find-min / find-max match the list" TRIALS random-case
  (lambda (c)
    (or (null? (car c))
      (let
        ((t (case-tree c)))
        (and (= (find-min t) (apply min (car c)))
          (= (find-max t) (apply max (car c))))))))
(property "height is between minimum possible and size" TRIALS random-case
  (lambda (c)
    (let
      ((t (case-tree c))
        (n (length (car c))))
      (<= (min-height n) (height t) n))))
(property "remove-val removes exactly one occurrence" TRIALS random-case
  (lambda (c)
    (let
      ((t (remove-val (case-tree c) (cadr c))))
      (and (equal? (in-order t) (sort-list (remove-first (cadr c) (car c))))
        (valid-bst? t)))))
(property "remove-all-val removes every occurrence" TRIALS random-case
  (lambda (c)
    (let
      ((t (remove-all-val (case-tree c) (cadr c))))
      (and (equal? (in-order t) (sort-list (remove-every (cadr c) (car c))))
        (valid-bst? t)
        (not (search t (cadr c)))))))
(property "removing every value one by one empties the tree" TRIALS random-case
  (lambda (c)
    (empty? (fold-left remove-val (case-tree c) (car c)))))
(property "balance is balanced?, keeps values and has minimum height" TRIALS random-unbalanced-case
  (lambda (c)
    (let
      ((b (balance (list->tree (car c)))))
      (and (balanced? b)
        (equal? (in-order b) (sort-list (car c)))
        (= (height b) (min-height (length (car c))))))))
(property "balanced? matches a direct definition" TRIALS random-unbalanced-case
  (lambda (c)
    (letrec
      ((slow (lambda (t)
              (or (empty? t)
                (and (<= (abs (- (height (get-left t)) (height (get-right t)))) 1)
                  (slow (get-left t))
                  (slow (get-right t)))))))
      (let
        ((t (list->tree (car c))))
        (eq? (balanced? t) (slow t))))))

;;; ------------------------------------------------------------------
;;; summary
;;; ------------------------------------------------------------------

(newline)
(display (format "~a passed, ~a failed" passed failed))
(newline)
(exit (if (zero? failed) 0 1))
