;; Tests for Sequences.scm
;; run with: scheme --script SequencesTests.scm
;; exits with 0 if every test passes, 1 otherwise
;;
;; The tests build their own sequences instead of using from / list->seq, so a
;; bug in one function doesn't make the tests of the others fail too:
;;   (make-q '(1 2 3))      the sequence 1, 2, 3
;;   (nats 5)               the infinite sequence 5, 6, 7, ...
;;   (bomb-after '(1 2))    the sequence 1, 2, then raises an error if anything asks
;;                          for the rest; used to check a function doesn't evaluate
;;                          more of a sequence than it needs to
;; Sequences returned by the functions are turned into lists for comparison by:
;;   (q->list s)            all the values of s (s must be finite)
;;   (q-prefix s n)         the first n values of s (or fewer if s is shorter)

(load (let
        ((dir (path-parent (car (command-line)))))
        (if (string=? dir "")
            "Sequences.scm"
            (string-append dir "/Sequences.scm"))))

;;; ------------------------------------------------------------------
;;; test framework
;;; ------------------------------------------------------------------

;; how much work a single test may do before it counts as stuck in an infinite loop
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

;; (run-test name expr thunk): runs thunk, which returns (got . want) as printed
;; strings, and passes if they match; errors and infinite loops count as failures
(define run-test
  (lambda (name expr thunk)
    (let
      ((r (run-limited thunk FUEL)))
      (case (car r)
        ((ok) (if (string=? (cadr r) (cddr r))
                  (set! passed (+ passed 1))
                  (report-fail name (format "~s => ~a, expected ~a" expr (cadr r) (cddr r)))))
        ((error) (report-fail name (format "raised: ~a  in ~s" (cdr r) expr)))
        (else (report-fail name (format "did not finish (infinite loop?)  in ~s" expr)))))))

;; (test name expr expected): passes if expr evaluates to something that prints
;; the same as expected
(define-syntax test
  (syntax-rules ()
    ((_ name expr expected)
     (run-test name 'expr
               (lambda () (cons (format "~s" expr) (format "~s" expected)))))))

;; (test-true name expr) / (test-false name expr): shorthands for boolean results
(define-syntax test-true
  (syntax-rules ()
    ((_ name expr) (test name (if expr #t #f) #t))))

(define-syntax test-false
  (syntax-rules ()
    ((_ name expr) (test name (if expr #t #f) #f))))

;; (property name trials gen describe check): runs (check (gen)) trials times;
;; counts as one test, and on failure prints (describe input) for the first
;; input it fails on
(define property
  (lambda (name trials gen describe check)
    (let loop ((i 0))
      (if (= i trials)
          (set! passed (+ passed 1))
          (let*
            ((input (gen))
             (r (run-limited (lambda () (check input)) FUEL)))
            (case (car r)
              ((ok) (if (cdr r)
                        (loop (+ i 1))
                        (report-fail name (describe input))))
              ((error) (report-fail name (format "~a~%        raised: ~a" (describe input) (cdr r))))
              (else (report-fail name (format "~a~%        did not finish (infinite loop?)" (describe input))))))))))

;; (section title): prints a section header
(define section
  (lambda (title)
    (newline)
    (display "== ")
    (display title)
    (newline)))

;;; ------------------------------------------------------------------
;;; helpers (independent of the code being tested)
;;; ------------------------------------------------------------------

;; (check-seq s): raises a readable error unless s looks like a sequence
(define check-seq
  (lambda (s)
    (unless (or (null? s)
                (and (pair? s) (procedure? (cdr s))))
      (error #f (format "expected a sequence ('() or (value . function)), got ~s" s)))))

;; (q-prefix s n): list of the first n values of s, or all of them if s is shorter
(define q-prefix
  (lambda (s n)
    (check-seq s)
    (if (or (zero? n) (null? s))
        '()
        (cons (car s) (q-prefix ((cdr s)) (- n 1))))))

;; (q->list s): list of all values of s; gives up on sequences over MAX-LENGTH long
(define MAX-LENGTH 10000)

(define q->list
  (lambda (s)
    (let
      ((l (q-prefix s (+ MAX-LENGTH 1))))
      (if (> (length l) MAX-LENGTH)
          (error #f (format "sequence has over ~a values; expected it to end" MAX-LENGTH))
          l))))

;; (make-q l): sequence of the values of list l
(define make-q
  (lambda (l)
    (if (null? l)
        '()
        (cons (car l) (lambda () (make-q (cdr l)))))))

;; (nats n): infinite sequence n, n+1, n+2, ...
(define nats
  (lambda (n)
    (cons n (lambda () (nats (+ n 1))))))

;; (bomb-after l): sequence of the values of l, which raises an error if
;; anything asks for the rest of the sequence after the last of them
(define bomb-after
  (lambda (l)
    (if (null? (cdr l))
        (cons (car l)
              (lambda ()
                (error #f (format "evaluated the sequence past ~s; only needed up to there" (car l)))))
        (cons (car l) (lambda () (bomb-after (cdr l)))))))

;; (pad l n): l with zeroes added at the end to make it n long
(define pad
  (lambda (l n)
    (append l (make-list (max 0 (- n (length l))) 0))))

;; (add-lists l1 l2): l1 and l2 added element-wise, the shorter padded with zeroes
(define add-lists
  (lambda (l1 l2)
    (let
      ((n (max (length l1) (length l2))))
      (map + (pad l1 n) (pad l2 n)))))

;; (interleave-lists l1 l2): l1 and l2 interleaved, the rest of the longer at the end
(define interleave-lists
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (interleave-lists l2 (cdr l1))))))

;; (range-list b e s): list of the values (range b e s) should produce, for s /= 0
(define range-list
  (lambda (b e s)
    (if (if (positive? s) (> b e) (< b e))
        '()
        (cons b (range-list (+ b s) e s)))))

;; (list-drop l n): l without its first n values, or '() if l is shorter
(define list-drop
  (lambda (l n)
    (if (or (zero? n) (null? l))
        l
        (list-drop (cdr l) (- n 1)))))

;; random inputs
(define TRIALS 500)
(random-seed 20261021)

(define random-int
  (lambda (lo hi)
    (+ lo (random (+ (- hi lo) 1)))))

(define random-list
  (lambda ()
    (let loop ((n (random 13)) (acc '()))
      (if (zero? n)
          acc
          (loop (- n 1) (cons (random-int -20 20) acc))))))

(define random-two-lists
  (lambda ()
    (list (random-list) (random-list))))

(define describe-list
  (lambda (l)
    (format "fails for the sequence of ~s" l)))

(define describe-two-lists
  (lambda (ls)
    (format "fails for the sequences of ~s and ~s" (car ls) (cadr ls))))

;;; ------------------------------------------------------------------
;;; tests
;;; ------------------------------------------------------------------

(section "from")
(test "from 1" (q-prefix (from 1) 5) '(1 2 3 4 5))
(test "from 0" (q-prefix (from 0) 3) '(0 1 2))
(test "from a negative number" (q-prefix (from -2) 5) '(-2 -1 0 1 2))
(test "from a large number" (q-prefix (from 1000000) 2) '(1000000 1000001))
(test-true "from returns a sequence" (let ((s (from 1))) (and (pair? s) (procedure? (cdr s)))))
(test "from goes on for a long time" (car (list-tail (q-prefix (from 0) 5000) 4999)) 4999)

(section "range")
(test "positive step" (q->list (range 1 5 1)) '(1 2 3 4 5))
(test "end is included when reached exactly" (q->list (range 1 10 3)) '(1 4 7 10))
(test "end is not reached exactly" (q->list (range 1 9 3)) '(1 4 7))
(test "negative step" (q->list (range 10 1 -3)) '(10 7 4 1))
(test "negative start" (q->list (range -3 3 2)) '(-3 -1 1 3))
(test "negative start and end" (q->list (range -1 -7 -2)) '(-1 -3 -5 -7))
(test "start equals end" (q->list (range 3 3 1)) '(3))
(test "start equals end, negative step" (q->list (range 3 3 -1)) '(3))
(test "start past end, positive step" (q->list (range 5 1 1)) '())
(test "start past end, negative step" (q->list (range 1 5 -1)) '())
(test "step larger than the range" (q->list (range 1 3 10)) '(1))
(test "end +inf.0" (q-prefix (range 0 +inf.0 2) 5) '(0 2 4 6 8))
(test "end -inf.0" (q-prefix (range 0 -inf.0 -2) 5) '(0 -2 -4 -6 -8))
(test "end +inf.0 with negative step is empty" (q->list (range 0 +inf.0 -1)) '())
(test "end -inf.0 with positive step is empty" (q->list (range 0 -inf.0 1)) '())
(test "step 0 repeats the start" (q-prefix (range 4 10 0) 4) '(4 4 4 4))
(test "step 0 never passes the end" (q-prefix (range 5 1 0) 3) '(5 5 5))
(test "values stay exact" (q->list (range 1 3 1)) '(1 2 3))
(property "range matches a direct definition" TRIALS
  (lambda ()
    (let
      ((s (random-int -4 3)))
      (list (random-int -10 10) (random-int -15 15) (if (zero? s) 4 s))))
  (lambda (in) (format "fails for (range ~s ~s ~s)" (car in) (cadr in) (caddr in)))
  (lambda (in) (equal? (q->list (apply range in)) (apply range-list in))))

(section "take")
(test "take from a finite sequence" (take (make-q '(1 2 3 4)) 2) '(1 2))
(test "take all values" (take (make-q '(1 2 3)) 3) '(1 2 3))
(test "take more than there are" (take (make-q '(1 2)) 5) '(1 2))
(test "take 0" (take (make-q '(1 2 3)) 0) '())
(test "take from empty" (take '() 3) '())
(test "take from an infinite sequence" (take (nats 1) 4) '(1 2 3 4))
(test-true "take returns a list" (list? (take (nats 1) 3)))
(test "take keeps non-number values" (take (make-q '(a "b" (c))) 3) '(a "b" (c)))
(test "take evaluates no further than it needs" (take (bomb-after '(1 2 3)) 2) '(1 2))

(section "list->seq")
(test "list->seq of empty is empty" (list->seq '()) '())
(test "list->seq values in order" (q->list (list->seq '(1 2 3))) '(1 2 3))
(test "list->seq of one value" (q->list (list->seq '(7))) '(7))
(test "list->seq keeps nested lists as values" (q->list (list->seq '((1 2) () (3)))) '((1 2) () (3)))
(test-true "list->seq returns a sequence" (let ((s (list->seq '(1 2)))) (and (pair? s) (procedure? (cdr s)))))
(property "list->seq gives back the list" TRIALS random-list describe-list
  (lambda (l) (equal? (q->list (list->seq l)) l)))

(section "mapq")
(test "mapq on empty" (mapq '() (lambda (x) (* x x))) '())
(test "mapq squares" (q->list (mapq (make-q '(1 2 3)) (lambda (x) (* x x)))) '(1 4 9))
(test "mapq on an infinite sequence" (q-prefix (mapq (nats 1) (lambda (x) (* 10 x))) 3) '(10 20 30))
(test "mapq can change the type of values" (q->list (mapq (make-q '(1 2)) number->string)) '("1" "2"))
(test "mapq is lazy" (car (mapq (bomb-after '(1)) (lambda (x) (+ x 1)))) 2)
(test "mapq calls f in order" (let ((seen '()))
                                (q->list (mapq (make-q '(1 2 3)) (lambda (x) (set! seen (cons x seen)) x)))
                                (reverse seen))
      '(1 2 3))
(property "mapq matches map" TRIALS random-list describe-list
  (lambda (l) (equal? (q->list (mapq (make-q l) (lambda (x) (- (* 2 x) 1)))) (map (lambda (x) (- (* 2 x) 1)) l))))

(section "filterq")
(test "filterq on empty" (filterq '() even?) '())
(test "filterq keeps matching values in order" (q->list (filterq (make-q '(1 2 3 4 5 6)) even?)) '(2 4 6))
(test "filterq when nothing matches" (filterq (make-q '(1 3 5)) even?) '())
(test "filterq when everything matches" (q->list (filterq (make-q '(2 4)) even?)) '(2 4))
(test "filterq on an infinite sequence" (q-prefix (filterq (nats 1) (lambda (x) (zero? (modulo x 7)))) 3) '(7 14 21))
(test "filterq skips a long run of non-matches" (car (filterq (nats 0) (lambda (x) (> x 5000)))) 5001)
(test "filterq is lazy" (car (filterq (bomb-after '(1 2 3)) even?)) 2)
(property "filterq matches filter" TRIALS random-list describe-list
  (lambda (l) (equal? (q->list (filterq (make-q l) positive?)) (filter positive? l))))

(section "foldlq")
(test "foldlq on empty gives the initial value" (foldlq + 0 '()) 0)
(test "foldlq sums" (foldlq + 0 (make-q '(1 2 3 4))) 10)
(test "foldlq folds from the left" (foldlq - 0 (make-q '(1 2 3))) -6)
(test "foldlq argument order: f(acc, x)" (foldlq list 'i (make-q '(1 2 3))) '(((i 1) 2) 3))
(test "foldlq can reverse a list" (foldlq (lambda (acc x) (cons x acc)) '() (make-q '(1 2 3))) '(3 2 1))
(test "foldlq of one value" (foldlq - 10 (make-q '(3))) 7)
(property "foldlq matches fold-left" TRIALS random-list describe-list
  (lambda (l) (equal? (foldlq list 'i (make-q l)) (fold-left list 'i l))))

(section "foldrq")
(test "foldrq on empty gives the initial value" (foldrq + '() 0) 0)
(test "foldrq sums" (foldrq + (make-q '(1 2 3 4)) 0) 10)
(test "foldrq folds from the right" (foldrq - (make-q '(1 2 3)) 0) 2)
(test "foldrq argument order: f(x, acc)" (foldrq list (make-q '(1 2 3)) 'i) '(1 (2 (3 i))))
(test "foldrq with cons copies the values" (foldrq cons (make-q '(1 2 3)) '()) '(1 2 3))
(test "foldrq of one value" (foldrq - (make-q '(3)) 10) -7)
(property "foldrq matches fold-right" TRIALS random-list describe-list
  (lambda (l) (equal? (foldrq list (make-q l) 'i) (fold-right list 'i l))))

(section "concatq")
(test "concatq of two empties" (concatq '() '()) '())
(test "concatq with empty first" (q->list (concatq '() (make-q '(1 2)))) '(1 2))
(test "concatq with empty second" (q->list (concatq (make-q '(1 2)) '())) '(1 2))
(test "concatq keeps order" (q->list (concatq (make-q '(1 2)) (make-q '(3 4)))) '(1 2 3 4))
(test "concatq with infinite second" (q-prefix (concatq (make-q '(a b)) (nats 1)) 5) '(a b 1 2 3))
(test "concatq with infinite first never reaches second" (q-prefix (concatq (nats 1) (make-q '(x))) 3) '(1 2 3))
(test "concatq is lazy" (car (concatq (bomb-after '(1)) (make-q '(2)))) 1)
(property "concatq matches append" TRIALS random-two-lists describe-two-lists
  (lambda (ls) (equal? (q->list (concatq (make-q (car ls)) (make-q (cadr ls)))) (append (car ls) (cadr ls)))))

(section "addq")
(test "addq of two empties" (addq '() '()) '())
(test "addq of equal lengths" (q->list (addq (make-q '(1 2 3)) (make-q '(10 20 30)))) '(11 22 33))
(test "addq with shorter second" (q->list (addq (make-q '(1 2 3)) (make-q '(10)))) '(11 2 3))
(test "addq with shorter first" (q->list (addq (make-q '(1)) (make-q '(10 20 30)))) '(11 20 30))
(test "addq with empty first" (q->list (addq '() (make-q '(1 2)))) '(1 2))
(test "addq with empty second" (q->list (addq (make-q '(1 2)) '())) '(1 2))
(test "addq of two infinite sequences" (q-prefix (addq (nats 0) (nats 100)) 3) '(100 102 104))
(test "addq of infinite and finite" (q-prefix (addq (nats 1) (make-q '(10 10))) 4) '(11 12 3 4))
(test "addq is lazy" (car (addq (bomb-after '(1)) (bomb-after '(2)))) 3)
(property "addq matches padded element-wise addition" TRIALS random-two-lists describe-two-lists
  (lambda (ls) (equal? (q->list (addq (make-q (car ls)) (make-q (cadr ls)))) (add-lists (car ls) (cadr ls)))))

(section "interleaveq")
(test "interleaveq of two empties" (interleaveq '() '()) '())
(test "interleaveq of equal lengths" (q->list (interleaveq (make-q '(1 2 3)) (make-q '(a b c)))) '(1 a 2 b 3 c))
(test "interleaveq with longer first" (q->list (interleaveq (make-q '(1 2 3 4)) (make-q '(a)))) '(1 a 2 3 4))
(test "interleaveq with longer second" (q->list (interleaveq (make-q '(1)) (make-q '(a b c)))) '(1 a b c))
(test "interleaveq with empty first" (q->list (interleaveq '() (make-q '(a b)))) '(a b))
(test "interleaveq with empty second" (q->list (interleaveq (make-q '(1 2)) '())) '(1 2))
(test "interleaveq of two infinite sequences" (q-prefix (interleaveq (nats 0) (nats 100)) 6) '(0 100 1 101 2 102))
(test "interleaveq of infinite and finite" (q-prefix (interleaveq (make-q '(a b)) (nats 1)) 6) '(a 1 b 2 3 4))
(test "interleaveq is lazy" (car (interleaveq (bomb-after '(1)) (bomb-after '(2)))) 1)
(property "interleaveq matches a direct definition" TRIALS random-two-lists describe-two-lists
  (lambda (ls) (equal? (q->list (interleaveq (make-q (car ls)) (make-q (cadr ls))))
                       (interleave-lists (car ls) (cadr ls)))))

(section "dropq")
(test "dropq 0" (q->list (dropq (make-q '(1 2 3)) 0)) '(1 2 3))
(test "dropq some" (q->list (dropq (make-q '(1 2 3 4)) 2)) '(3 4))
(test "dropq all" (dropq (make-q '(1 2 3)) 3) '())
(test "dropq more than there are" (dropq (make-q '(1 2)) 5) '())
(test "dropq from empty" (dropq '() 2) '())
(test "dropq from an infinite sequence" (q-prefix (dropq (nats 1) 10) 3) '(11 12 13))
(test "dropq returns a sequence, not a list" (q->list (dropq (make-q '(1 2 3)) 1)) '(2 3))
(test "dropq evaluates no further than it needs" (car (dropq (bomb-after '(1 2 3)) 2)) 3)
(property "dropq matches dropping from the list" TRIALS
  (lambda () (list (random-list) (random 15)))
  (lambda (in) (format "fails for dropping ~s from the sequence of ~s" (cadr in) (car in)))
  (lambda (in) (equal? (q->list (dropq (make-q (car in)) (cadr in))) (list-drop (car in) (cadr in)))))

(section "functions working together")
(test "take of range" (take (range 1 100 7) 3) '(1 8 15))
(test "take of mapq of from" (take (mapq (from 1) (lambda (x) (* x x))) 5) '(1 4 9 16 25))
(test "take of filterq of from" (take (filterq (from 1) odd?) 4) '(1 3 5 7))
(test "foldlq of range" (foldlq + 0 (range 1 100 1)) 5050)
(test "foldrq of list->seq" (foldrq cons (list->seq '(1 2 3)) '()) '(1 2 3))
(test "take of dropq of from" (take (dropq (from 0) 5) 3) '(5 6 7))
(test "evens interleaved with odds" (take (interleaveq (range 0 +inf.0 2) (range 1 +inf.0 2)) 6) '(0 1 2 3 4 5))
(test "take of concatq of ranges" (take (concatq (range 1 3 1) (range 10 0 -5)) 10) '(1 2 3 10 5 0))
(test "addq of from and itself" (take (addq (from 1) (from 1)) 4) '(2 4 6 8))
(test "sum of the squares of the first 10 odd numbers"
      (foldlq + 0 (list->seq (take (mapq (filterq (from 1) odd?) (lambda (x) (* x x))) 10)))
      1330)
(test "fibonacci with addq"
      (letrec
        ((fib (cons 0 (lambda () (cons 1 (lambda () (addq fib ((cdr fib)))))))))
        (take fib 10))
      '(0 1 1 2 3 5 8 13 21 34))

;;; ------------------------------------------------------------------
;;; summary
;;; ------------------------------------------------------------------

(newline)
(display (format "~a passed, ~a failed" passed failed))
(newline)
(exit (if (zero? failed) 0 1))
