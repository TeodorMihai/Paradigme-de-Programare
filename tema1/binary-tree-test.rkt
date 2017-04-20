;;#lang racket

(require test-engine/racket-tests)

(define default-results '(#f 0 () your-code-here)) ; ce rezultate default sunt întoarse în exerciții
(define show-defaults 2) ; câte exerciții la care s-au întors rezultate default să fie arătate detaliat
(define prepend #t) (define name-ex '(task . taskurile)) ; variante: '(exercițiul . exercițiile) sau '(testul . testele) sau '(task . taskurile)
(define : 'separator) (define punct 'string) (define puncte 'string) (define BONUS 'string) (define total 0) (define n-ex 0) (define p-ex 0) (define defaults '())
(define (ex n sep p . s) (set! n-ex n) (set! p-ex p)) (define exercițiul ex) (define (p L) (map (λ (e) (display e) (display " ")) L) (newline))
(define (check-exp given expected) (check-exp-part "" 1 given expected)) (define (check-exp-part part percent given expected) (if (not (equal? given expected)) (and
  (when (member given default-results) (set! defaults (cons (if (< percent 1) (list n-ex part) n-ex) defaults)))
  (when (<= (length defaults) show-defaults) (p `(NU: la ,(car name-ex) ,n-ex ,(if (< percent 1) part "") rezultatul ,given diferă de cel așteptat: ,expected))))
 (let ((pts (* p-ex percent))) (and (if prepend (printf "+~v: " pts) (printf "OK: "))
  (p `(,(car name-ex) ,(if (< percent 1) (list n-ex part) n-ex) rezolvat: + ,pts ,(if (= pts 1) 'punct 'puncte))) (set! total (+ total pts))))))
(define (sumar) (when (and (not (null? defaults)) (< show-defaults (length defaults))) (p `(... rezultatul implicit dat la ,(cdr name-ex) ,(reverse defaults)))) (p `(total: ,(round total) puncte)))
(define Task ex) (define Bonus ex)
(define create-tree
  (λ (list fn)
    (let [(is-node? (list-ref fn 0)) (is-leaf? (list-ref fn 1)) (is-empty? (list-ref fn 2)) (get-value (list-ref fn 3)) (make-node (list-ref fn 4))
                                     (get-right (list-ref fn 5)) (get-left (list-ref fn 6)) (inorder (list-ref fn 7)) (height (list-ref fn 8))
                                     (insert (list-ref fn 9))]
      (foldl (λ (x y) (insert y x)) null list))))
(define get-set
  (λ (tree fn)
    (let [(is-node? (list-ref fn 0)) (is-leaf? (list-ref fn 1)) (is-empty? (list-ref fn 2)) (get-value (list-ref fn 3)) (make-node (list-ref fn 4))
                                     (get-right (list-ref fn 5)) (get-left (list-ref fn 6)) (inorder (list-ref fn 7)) (height (list-ref fn 8))
                                     (insert (list-ref fn 9))]
      (if (check-binary-search-tree tree fn)
          (inorder tree)
          '()))))

(define check-binary-search-tree
  (λ (tree fn)
    (let [(is-node? (list-ref fn 0)) (is-leaf? (list-ref fn 1)) (is-empty? (list-ref fn 2)) (get-value (list-ref fn 3)) (make-node (list-ref fn 4))
                                     (get-right (list-ref fn 5)) (get-left (list-ref fn 6)) (inorder (list-ref fn 7)) (height (list-ref fn 8))
                                     (insert (list-ref fn 9))]
      (if (is-empty? tree)
          #t
          (if (is-node? tree)
              (if (is-leaf? tree)
                  #t
                  (and (if (is-empty? (get-left tree))
                           #t
                           (> (get-value tree) (get-value (get-left tree))))
                       (if (is-empty? (get-right tree))
                           #t
                           (< (get-value tree) (get-value (get-right tree))))
                       (check-binary-search-tree (get-left tree) fn)
                       (check-binary-search-tree (get-right tree) fn)))
            #f)))))
(define check-expression-tree
  (λ (tree fn)
     (let [(is-node? (list-ref fn 0)) (is-leaf? (list-ref fn 1)) (is-empty? (list-ref fn 2)) (get-value (list-ref fn 3)) (make-node (list-ref fn 4))
                                     (get-right (list-ref fn 5)) (get-left (list-ref fn 6)) (inorder (list-ref fn 7)) (height (list-ref fn 8))
                                     (insert (list-ref fn 9))]
       (if (is-empty? tree)
           #t
           (if (is-node? tree)
               (if (is-leaf? tree)
                   (number? (get-value tree))
                   (and (not (is-empty? (get-left tree))) (not (is-empty? (get-right tree)))
                        (or (equal? (get-value tree) '+) (equal? (get-value tree) '-)
                            (equal? (get-value tree) '*) (equal? (get-value tree) '/))
                        (check-expression-tree (get-left tree) fn) (check-expression-tree (get-right tree) fn)))
               #f)))))
(define test-bonus
  (λ (tree fn)
    (if (check-expression-tree tree fn)
        (evaluate tree)
        0)))
(define test-task1
  (λ (tree fn)
    (if (check-binary-search-tree tree fn)
        (get-set tree fn)
        '())))
(define check-self-balancing-tree
  (λ (tree fn)
     (let [(is-node? (list-ref fn 0)) (is-leaf? (list-ref fn 1)) (is-empty? (list-ref fn 2)) (get-value (list-ref fn 3)) (make-node (list-ref fn 4))
                                     (get-right (list-ref fn 5)) (get-left (list-ref fn 6)) (inorder (list-ref fn 7)) (height (list-ref fn 8))
                                     (insert (list-ref fn 9))]
       (if (check-binary-search-tree tree fn)
           (if (is-leaf? tree)
               #t
               (let [(lHeight (height (get-left tree))) (rHeight (height (get-right tree)))]
                 (<= (abs (- lHeight rHeight)) 1)))
           #f))))

;; TASK (pregătitor):
;; Definiți (manual), în formatul ales, arborele binar de căutare reprezentat mai jos.
;;         9
;;        / \
;;       /   \
;;      /     \
;;     3      12
;;    / \     / \
;;   2   6   11 15
;;  /   / \     / \
;; 1   5   8   13 21
;;        /
;;       7


;; Task 1:
;; Implementați operațiile specifice mulțimilor, utilizând, ca formă de reprezentare, arborele binar de căutare.
;; Atenție! Trebuie să vă asigurați că arborele binar rezultat este echilibrat.
(define result-union4
  '(-5552 -815 -8 0 1 2 4 5 6 7 8 9 10 14 15 18 22 24 25 42 45 51 52 54 61 64 78 89 92 94 95 97 100 142 151 155 214 215 216 251 416 478 485 545 644 651 655 679 789 810 842 846 942 955 2465 2541 2542 3541 4511 5254 5488 7891 8542 8755 8891))
(define result-check-set1 '(-6 -5 1 2 3 4 5 6 8 9 10 11 17))
(define result-check-set2 '(-9 0 1 2 3 4 5 7 8 9 11 12 15 35 51 251 481 6551))
(define result-check-set3 '(-8 0 1 2 4 5 7 8 9 10 15 45 51 61 142 151 215 416 644 651 2542 3541 8542))
(define result-check-set4 '(-815 0 2 4 5 7 8 10 25 54 64 94 214 216 251 485 644 942 2541 2542 4511 5254))
(define result-check-set5 '(-5552 0 1 4 5 8 14 15 25 42 54 78 97 155 545 679 789 810 842 846 955 2465 5488 7891 8755 8891))
(define result-union1 '(-815 -8 0 1 2 4 5 7 8 9 10 15 25 45 51 54 61 64 94 142 151 214 215 216 251 416 485 644 651 942 2541 2542 3541 4511 5254 8542))
(define result-union2 '(-5552 0 1 4 5 6 7 8 9 14 15 18 22 24 25 42 52 54 78 89 92 94 95 97 100 155 478 485 545 655 679 789 810 842 846 955 2465 5488 7891 8755 8891))
(define result-union3 '(-8 0 1 2 4 5 6 7 8 9 10 15 18 22 24 45 51 52 61 78 89 92 94 95 97 100 142 151 215 416 478 485 644 651 655 2542 3541 8542))
(define result-intersection1 '(0 2 4 5 7 8 10 644 2542))
(define result-intersection2 '(0 4 5 8 25 54))
(define result-intersection3 '(1 4 5 8 78 97))
(define result-intersection4 '(4 5 8))
(define result-complements1 '(-8 1 9 15 45 51 61 142 151 215 416 651 3541 8542))
(define result-complements2 '(-5552 0 14 15 25 42 54 155 545 679 789 810 842 846 955 2465 5488 7891 8755 8891))
(define result-complements3 '(-5552 0 14 15 25 42 54 155 545 679 789 810 842 846 955 2465 5488 7891 8755 8891))
(define result-complements4 '(-8 1 9 45 51 61 142 151 215 416 651 3541 8542))
(define result-insert1 '(-815 -7 0 2 4 5 7 8 10 25 54 64 94 214 216 251 485 644 942 2541 2542 4511 5254))
(define result-insert2 '(-8 0 1 2 4 5 7 8 9 10 15 45 51 61 142 151 215 416 644 651 2542 3541 8542 59525))
(define result-insert3 '(-5552 0 1 4 5 8 14 15 25 42 54 78 97 155 545 679 789 810 842 846 955 2465 5488 7891 8755 8891 988522))
(define result-insert4 '(-812612 1 4 5 6 7 8 9 18 22 24 52 78 89 92 94 95 97 100 478 485 655))
(define result-remove1 '(2 3 5 6 7 8 9 11 12 13 15 21))
(define result-remove2 '(1 2 3 5 6 7 8 11 12 13 15 21))
(define result-remove3 '(1 2 5 6 7 8 9 11 12 13 15 21))
(define result-remove4 '(0 1 2 4 5 7 8 9 10 45 51 61 142 151 215 644 651 2542 3541 8542))
(define result-complex1 '(-8 0 1 2 4 5 7 8 9 10 15 25 45 51 54 61 142 151 215 416 644 651 2542 3541 8542))
(define result-complex2 '(-815 2 7 10 64 94 214 216 251 485 644 942 2541 2542 4511 5254 7851))
(define result-complex3 '(-815 1 2 3 5 6 7 8 9 11 12 13 15 21 64 94 216 251 485 942 2541 4511 5254))
(define result-complex4 '(-8 0 2 10 15 25 45 51 54 61 94 142 151 215 416 485 644 651 2542 3541 8542))
(define result-complex5 '(0))
(define result-complex6 '(0 1 4 5 8 9 15 22 45 51 61 78 97 142 151 215 416 651 3541 8542))
(define result-complex7 '(-8 0 1 2 4 5 7 8 10 15 45 51 61 94 142 151 215 416 485 644 651 2542 3541 8542))
(define result-complex8 '(-5552 -815 -8 0 1 2 4 5 6 7 8 9 10 14 15 18 22 24 25 42 45 51 52 54 61 64 78 89 92 94 95 97 100 142 151 155 214 215 216 251 416 478 485 545 644 651 655 679 789 810 842 846 942 955 2465 2541 2542 3541 4511 5254 5488 7891 8542 8755 8891))
(define result-complex9 '(1 9 15 25 54 94 485))
(define result-complex10 '(94 100))

;; Task 2:
;; Implementați funcțiile care determină submulțimile cu proprietățile specificate în enunț.
(define result-k-subsets1 '((0 2 4 5 7 8 10 644) (0 2 4 5 7 8 10 2542) (0 2 4 5 7 8 644 2542) (0 2 4 5 7 10 644 2542) (0 2 4 5 8 10 644 2542) (0 2 4 7 8 10 644 2542) (0 2 5 7 8 10 644 2542) (0 4 5 7 8 10 644 2542) (2 4 5 7 8 10 644 2542)))
(define result-k-subsets3 '((1 2 3) (1 2 4) (1 2 5) (1 3 4) (1 3 5) (1 4 5) (2 3 4) (2 3 5) (2 4 5) (3 4 5)))
(define result-k-subsets4 '((5 6) (5 7) (5 8) (6 7) (6 8) (7 8)))
(define result-k-subsets5 '((1 4 5 6 7 8 9 18 22 24 52 78 89 92 94 95 97 100 478 485)
  (1 4 5 6 7 8 9 18 22 24 52 78 89 92 94 95 97 100 478 655)
  (1 4 5 6 7 8 9 18 22 24 52 78 89 92 94 95 97 100 485 655)
  (1 4 5 6 7 8 9 18 22 24 52 78 89 92 94 95 97 478 485 655)
  (1 4 5 6 7 8 9 18 22 24 52 78 89 92 94 95 100 478 485 655)
  (1 4 5 6 7 8 9 18 22 24 52 78 89 92 94 97 100 478 485 655)
  (1 4 5 6 7 8 9 18 22 24 52 78 89 92 95 97 100 478 485 655)
  (1 4 5 6 7 8 9 18 22 24 52 78 89 94 95 97 100 478 485 655)
  (1 4 5 6 7 8 9 18 22 24 52 78 92 94 95 97 100 478 485 655)
  (1 4 5 6 7 8 9 18 22 24 52 89 92 94 95 97 100 478 485 655)
  (1 4 5 6 7 8 9 18 22 24 78 89 92 94 95 97 100 478 485 655)
  (1 4 5 6 7 8 9 18 22 52 78 89 92 94 95 97 100 478 485 655)
  (1 4 5 6 7 8 9 18 24 52 78 89 92 94 95 97 100 478 485 655)
  (1 4 5 6 7 8 9 22 24 52 78 89 92 94 95 97 100 478 485 655)
  (1 4 5 6 7 8 18 22 24 52 78 89 92 94 95 97 100 478 485 655)
  (1 4 5 6 7 9 18 22 24 52 78 89 92 94 95 97 100 478 485 655)
  (1 4 5 6 8 9 18 22 24 52 78 89 92 94 95 97 100 478 485 655)
  (1 4 5 7 8 9 18 22 24 52 78 89 92 94 95 97 100 478 485 655)
  (1 4 6 7 8 9 18 22 24 52 78 89 92 94 95 97 100 478 485 655)
  (1 5 6 7 8 9 18 22 24 52 78 89 92 94 95 97 100 478 485 655)
  (4 5 6 7 8 9 18 22 24 52 78 89 92 94 95 97 100 478 485 655)))
(define result-zig-zag1
                '((1 3 2 5 4 6) (1 3 2 6 4 5) (1 4 2 5 3 6) (1 4 2 6 3 5) (1 4 3 5 2 6)
                                (1 4 3 6 2 5) (1 5 2 4 3 6) (1 5 2 6 3 4) (1 5 3 4 2 6)
                                (1 5 3 6 2 4) (1 5 4 6 2 3) (1 6 2 4 3 5) (1 6 2 5 3 4)
                                (1 6 3 4 2 5) (1 6 3 5 2 4) (1 6 4 5 2 3) (2 3 1 5 4 6)
                                (2 3 1 6 4 5) (2 4 1 5 3 6) (2 4 1 6 3 5) (2 4 3 5 1 6)
                                (2 4 3 6 1 5) (2 5 1 4 3 6) (2 5 1 6 3 4) (2 5 3 4 1 6)
                                (2 5 3 6 1 4) (2 5 4 6 1 3) (2 6 1 4 3 5) (2 6 1 5 3 4)
                                (2 6 3 4 1 5) (2 6 3 5 1 4) (2 6 4 5 1 3) (3 4 1 5 2 6)
                                (3 4 1 6 2 5) (3 4 2 5 1 6) (3 4 2 6 1 5) (3 5 1 4 2 6)
                                (3 5 1 6 2 4) (3 5 2 4 1 6) (3 5 2 6 1 4) (3 5 4 6 1 2)
                                (3 6 1 4 2 5) (3 6 1 5 2 4) (3 6 2 4 1 5) (3 6 2 5 1 4)
                                (3 6 4 5 1 2) (4 5 1 3 2 6) (4 5 1 6 2 3) (4 5 2 3 1 6)
                                (4 5 2 6 1 3) (4 5 3 6 1 2) (4 6 1 3 2 5) (4 6 1 5 2 3)
                                (4 6 2 3 1 5) (4 6 2 5 1 3) (4 6 3 5 1 2) (5 6 1 3 2 4)
                                (5 6 1 4 2 3) (5 6 2 3 1 4) (5 6 2 4 1 3) (5 6 3 4 1 2)
                                (2 1 4 3 6 5) (2 1 5 3 6 4) (2 1 5 4 6 3) (2 1 6 3 5 4)
                                (2 1 6 4 5 3) (3 1 4 2 6 5) (3 1 5 2 6 4) (3 1 5 4 6 2)
                                (3 1 6 2 5 4) (3 1 6 4 5 2) (3 2 4 1 6 5) (3 2 5 1 6 4)
                                (3 2 5 4 6 1) (3 2 6 1 5 4) (3 2 6 4 5 1) (4 1 3 2 6 5)
                                (4 1 5 2 6 3) (4 1 5 3 6 2) (4 1 6 2 5 3) (4 1 6 3 5 2)
                                (4 2 3 1 6 5) (4 2 5 1 6 3) (4 2 5 3 6 1) (4 2 6 1 5 3)
                                (4 2 6 3 5 1) (4 3 5 1 6 2) (4 3 5 2 6 1) (4 3 6 1 5 2)
                                (4 3 6 2 5 1) (5 1 3 2 6 4) (5 1 4 2 6 3) (5 1 4 3 6 2)
                                (5 1 6 2 4 3) (5 1 6 3 4 2) (5 2 3 1 6 4) (5 2 4 1 6 3)
                                (5 2 4 3 6 1) (5 2 6 1 4 3) (5 2 6 3 4 1) (5 3 4 1 6 2)
                                (5 3 4 2 6 1) (5 3 6 1 4 2) (5 3 6 2 4 1) (5 4 6 1 3 2)
                                (5 4 6 2 3 1) (6 1 3 2 5 4) (6 1 4 2 5 3) (6 1 4 3 5 2)
                                (6 1 5 2 4 3) (6 1 5 3 4 2) (6 2 3 1 5 4) (6 2 4 1 5 3)
                                (6 2 4 3 5 1) (6 2 5 1 4 3) (6 2 5 3 4 1) (6 3 4 1 5 2)
                                (6 3 4 2 5 1) (6 3 5 1 4 2) (6 3 5 2 4 1) (6 4 5 1 3 2)
                                (6 4 5 2 3 1)))
(define result-zig-zag2
                '((1 3 2 4) (1 4 2 3) (2 3 1 4) (2 4 1 3) (3 4 1 2) (2 1 4 3) (3 1 4 2)
                            (3 2 4 1) (4 1 3 2) (4 2 3 1)))
(define result-zig-zag3
                '((1 7 5 10 9) (1 9 5 10 7) (1 9 7 10 5) (1 10 5 9 7) (1 10 7 9 5)
                               (5 7 1 10 9) (5 9 1 10 7) (5 9 7 10 1) (5 10 1 9 7)
                               (5 10 7 9 1) (7 9 1 10 5) (7 9 5 10 1) (7 10 1 9 5)
                               (7 10 5 9 1) (9 10 1 7 5) (9 10 5 7 1) (5 1 9 7 10)
                               (5 1 10 7 9) (7 1 9 5 10) (7 1 10 5 9) (7 5 9 1 10)
                               (7 5 10 1 9) (9 1 7 5 10) (9 1 10 5 7) (9 5 7 1 10)
                               (9 5 10 1 7) (9 7 10 1 5) (10 1 7 5 9) (10 1 9 5 7)
                               (10 5 7 1 9) (10 5 9 1 7) (10 7 9 1 5)))
(define result-zig-zag4
                '((-457 1 -85 98 5) (-457 5 -85 98 1) (-457 5 1 98 -85) (-457 98 -85 5 1)
                  (-457 98 1 5 -85) (-85 1 -457 98 5) (-85 5 -457 98 1) (-85 5 1 98 -457)
                  (-85 98 -457 5 1) (-85 98 1 5 -457) (1 5 -457 98 -85) (1 5 -85 98 -457)
                  (1 98 -457 5 -85) (1 98 -85 5 -457) (5 98 -457 1 -85) (5 98 -85 1 -457)
                  (-85 -457 5 1 98) (-85 -457 98 1 5) (1 -457 5 -85 98) (1 -457 98 -85 5)
                  (1 -85 5 -457 98) (1 -85 98 -457 5) (5 -457 1 -85 98) (5 -457 98 -85 1)
                  (5 -85 1 -457 98) (5 -85 98 -457 1) (5 1 98 -457 -85) (98 -457 1 -85 5)
                  (98 -457 5 -85 1) (98 -85 1 -457 5) (98 -85 5 -457 1) (98 1 5 -457 -85)))

;; BONUS
;; Definiți funcțiile care parsează și evalueaza o expresie, dată în forma infixată, uzitând arborele sintactic.
