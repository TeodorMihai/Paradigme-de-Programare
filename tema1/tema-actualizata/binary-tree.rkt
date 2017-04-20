
#lang racket
(require racket/include)
(include "binary-tree-test.rkt")


(define binary-search-tree (list (list (list (list null null 1) null 2)  (list (list null null 5) (list (list null null 7) null 8) 6) 3)
                             (list (list null null 11) (list (list null null 13) ( list null null 21) 15) 12)  9) )
;; TASK pregatitor


(define empty-tree
  null )


(define get-value
  (λ (node) (caddr node) ) )

(define get-left
  (λ (node)
    (if (null? node)
        null
        (car node) ) ) )

(define get-right
  (λ (node)
    (if (null? node)
        null
        (cadr node) ) ))



(define init-node 
  (λ (value) (list null null value) ) )


(define make-node
  (λ (left right value) (list left right value) ) )


(define is-leaf?
  (λ (node)
    (if (null? node)
        #f
        (and
         (list? node)
         (equal? (length node) 3 )
         (not (null? (get-value node) ) ) 
         (null? (get-left node) ) 
         (null? (get-right node) ) ) ) ) )

(define is-node?
  (λ (node)
    (if (null? node)
        #f
        (and
         (list? node)
         (equal? (length node) 3)
         (not (null? (get-value node) ) )))) )

(define is-empty?
  (λ (tree)
    (null? tree) ) )

(define has-left?
  (λ (tree)
    (not (null? (get-left tree) ) ) ) )

(define has-right?
  (λ (tree)
    (not (null? (get-right tree)))))

(define minimum
  (λ (tree)
    (if (not (has-left? tree))
        (get-value tree)
        (minimum (get-left tree)))))

(define maximum
  (λ (tree)
    (if (not (has-right? tree) )
        (get-value tree)
        (maximum (get-right tree)))))

(define height
  (λ (tree)
    (if (null? tree)
        0
        (+ 1 (max (height (get-left tree) ) (height (get-right tree) ) ) ) ) ) )

(define inorder
  (λ (tree)
    (if (is-empty? tree)
        null
        (let( [left (get-left tree)] [right (get-right tree)] [value (get-value tree)] )
          (cond
            [(and (is-empty? left) (is-empty? right))
             (list value)]
            [(and (not (is-empty? left) ) (is-empty? right) )
             (append (inorder left) (list value) ) ]
            [(and (is-empty? left) (not (is-empty? right) ) )
             (cons value (inorder right)  )]
            [(and (not (is-empty? left) ) (not (is-empty? right) ) )
             (append (inorder left) (cons value (inorder right)))])))))



(define preorder
  (λ (tree)
    (if (is-empty? tree)
        null
        (let ( [left (get-left tree)]  [right (get-right tree)] [value (get-value tree)] )
          (cond
            [(and (is-empty? left) (is-empty? right)
                  (list value) ) ]
            [(and (not (is-empty? left) ) (is-empty? right) )
             (cons value (preorder left) ) ]
            [(and (is-empty? left) (not (is-empty? right) ) )
             (cons value (preorder right) ) ]
            [(and (not (is-empty? left) ) (not (is-empty? right) ) )
             (cons value (append (preorder left) (preorder right) ) ) ])))))


(define postorder
  (λ (tree)
    (if (null? tree)
        null
        (let ( [left (get-left tree)] [right (get-right tree)] [value (get-value tree)] )
          (cond
            [(and (is-empty? left) (is-empty? right) )
             (list value) ]
            [(and (not (is-empty? left) ) (is-empty? right) )
             (append (postorder left) (list value) )]
            [(and (is-empty? left) (not (is-empty? right) ))
             (append (postorder right) (list value) )]
            [(and (not (is-empty? left) ) (not (is-empty? right) ))
             (append (append (postorder left) (postorder right)) (list value)) ] )))))


;; retin cel mai mic numar mai mare ca value, pe drumul meu, cand gasesc nodul cu value,
;; returnez minimul dintre ce am gasit pe lant pana la fiu si nodul candidat de la fiu in jos
;; (cel mai mic numar continut in arborele drerapta al nodului cu value
;; functioneaza si daca value nu este in arbore

(define get-successor
  (λ (tree value suc)
    (if (is-empty? tree)
        suc
        (let ( [ curent-value (get-value tree) ] )
          (cond
            [ (< value curent-value )
              (get-successor (get-left tree) value (if (< curent-value suc) curent-value suc) ) ]
            [ (> value curent-value )
              (get-successor (get-right tree) value suc) ]
            [ (= value curent-value)
              (if (is-empty? (get-right tree) )
                  suc
                  (min suc (minimum (get-right tree) )))])))))


(define successor
  (λ (tree value)
    (let ([suc (get-successor tree value 100000000000000)])
    (if (equal? suc  100000000000000 ) null suc) ) ) )


;;asemanator cu la successor, retin candidatul pala dau de nodul cu value, dar apoi returnez maximul din fiul stang

(define get-predecessor
  (λ (tree value pred)
    (if (is-empty? tree)
        pred
        (let ( [curent-value (get-value tree)] )
          (cond
            [ (< value curent-value)
              (get-predecessor (get-left tree) value pred) ]
            [ (> value curent-value)
              (get-predecessor (get-right tree) value (if (< pred curent-value) curent-value pred) ) ]
            [ (= value curent-value)
              (if (is-empty? (get-left tree) )
                  pred
                  (max pred (maximum (get-left tree) ) ) ) ])))))

(define predecessor
  (λ (tree value)
    (let ([pred (get-predecessor tree value -1000000000000000)])
      (if (equal? pred -1000000000000000) null pred ) )))

;;Task 1

(define contains
  (λ (tree value)
    (if (null? tree)
        #f
        (or
         (equal? (get-value tree) value)
         (if (< (get-value tree) value) 
             (contains (get-right tree) value)
             (contains (get-left tree) value))))))

(define insert
  (λ (tree value)
    (if (is-empty? tree)
        (init-node value)
        (let ([ left (get-left tree) ] [ right (get-right tree) ] [ node-value (get-value tree) ])
          (if (equal? node-value value)
              (balance (make-node left right node-value) )
              (if (< node-value value) 
                  (if (has-right? tree)
                      (balance (make-node left (insert right value) node-value) )
                      (balance (make-node left (init-node value) node-value) ) )
                  (if (has-left? tree) 
                      (balance (make-node (insert left value) right node-value) )
                      (balance (make-node (init-node value) right node-value) ) )))))))

;; urca nodul right in locul nodului curent, nodul curent devine fiul stanga al nodului right
;; rezulatul este in micsorarea diferentei intre subarborii right si left, right scanzand cu 1
;; nu trebuie ca tree sa fie null, si nici fiul dreapta al lui tree sa fie null
(define rotate-left
  (λ (tree)
    (let* ( [left (get-left tree)] [right (get-right tree)] [node-value (get-value tree)] 
                                   [right-left (get-left right)] [right-right (get-right right)] [right-value (get-value right)]
                                   [new-tree (make-node left right-left node-value)] )
      (make-node new-tree right-right right-value) ) ) )


(define rotate-right
  (λ (tree)
    (let* ( [left (get-left tree)] [right (get-right tree)] [node-value (get-value tree)]
                                   [left-left (get-left left)] [left-right (get-right left)] [left-value (get-value left)]
                                   [new-tree (make-node left-right right node-value)] )
      (make-node left-left new-tree left-value)) ))

(define balance
  (λ (tree) 
    (if (is-empty? tree)
        empty-tree
        (let ( [left (get-left tree)] [right (get-right tree)] [value (get-value tree)] )
          (if (> (height left) (+ (height right) 1)  )
              ; automat left nu e null
              (if (< (height (get-left left) ) (height (get-right left) ) )
                  ;automat left-right nu e null
                  (rotate-right (make-node (rotate-left left) right value ) ) ;LR
                  (rotate-right tree) ) ;LL
              
              (if (> (height right) (+ (height left) 1) )
                  ;autmat right nu e null
                  (if (> (height (get-left right))  (height (get-right right) ) )
                      ;automat right-left nu e null
                      (rotate-left (make-node left (rotate-right right) value) ) ;RL
                      (rotate-left tree) ) ;RR
                  tree))))))

(define union
  (λ (tree1 tree2)
    (if (null? tree1)
        tree2
        (let ( [left (get-left tree1)] [right (get-right tree1)] [value (get-value tree1)] )
           (union
           right
           (union left (insert tree2 value) )))))) 


(define iterative-intersection
  (λ (tree1 tree2 union-tree)
    (if (is-empty? tree1)
        union-tree
        (let ( [left (get-left tree1)] [right (get-right tree1)] [value (get-value tree1)] )
          (iterative-intersection
           right
           tree2
           (iterative-intersection
            left
            tree2
            (if (contains tree2 value) (insert union-tree value) union-tree ) ))))))


(define intersection
  (λ (tree1 tree2)
    (iterative-intersection tree1 tree2 empty-tree) ))

;;returneaza arborele care contine elementele din tree1 care NU se afla si in tree2
(define iterative-complements
  (λ (tree1 tree2 compl-tree)
    (if (is-empty? tree1)
        compl-tree
        (let* ([left (get-left tree1)] [right (get-right tree1)] [value (get-value tree1)]
                                       [value-in-tree (contains tree2 value)] )
          (iterative-complements
           right
           tree2
           (iterative-complements
            left
            tree2
            (if value-in-tree compl-tree (insert compl-tree value) )))))))


(define complements
  (λ (tree1 tree2)
    (iterative-complements tree1 tree2 empty-tree) ) )


;;intoarce arborele tree fara valoarea cea mai mare, niciodata tree nu e null, nu asiguram inainte sa intram in el ca nu e null
(define remove-max
  (λ (tree)
    (if (has-right? tree)
        (make-node (get-left tree) (remove-max (get-right tree) ) (get-value tree) )
        (get-left tree) )))

(define remove
  (λ (tree value)
    (if (is-empty? tree)
        '()
        (cond
          [(equal? (get-value tree) value)
           (cond
             [(is-leaf? tree) '()]
             [(and (has-left? tree) (not (has-right? tree))) (get-left tree)]
             [(and (has-right? tree) (not (has-left? tree))) (get-right tree)]
             [else (let t1-to-t2right ([t1 (get-right tree)] [t2 (get-left tree)])
                      (if (is-empty? t2)
                          t1
                          (make-node (get-left t2) (t1-to-t2right t1 (get-right t2)) (get-value t2))))])]
          [(> value (get-value tree)) 
           (make-node (get-left tree) (remove (get-right tree) value) (get-value tree))
           ]
          [(< value (get-value tree)) 
           (make-node (remove (get-left tree) value) (get-right tree) (get-value tree))
           ]
          )
     )
    )
  )



;(define remove
;  (λ (tree value)
;    (if (is-empty? tree)
;        empty-tree
;        (let ( [left (get-left tree)] [right (get-right tree)] [node-value (get-value tree)] )
;          (if (equal? node-value value)
;              (if (has-left? tree)
;                  (balance (make-node (remove-max left) right (maximum left) ) )
;                  right) 
;              (if (< value node-value)
;                  (balance (make-node (remove left value) right node-value) )
;                  (balance (make-node left (remove right value) node-value) ) ) )))))

;;Task 2

(define count-tree
  ( λ (tree)
     (if (is-empty? tree)
         0
         (+ 1 (+
              (count-tree (get-left tree) )
              (count-tree (get-right tree) ))))))

;(define k-subsets-raw
;  (λ (set k)
;    (cond
;      [(zero? k) '(())]
;      [(equal? (count-tree set) k) (list (inorder set) )] ;;daca arborele mai are doar cate valori mai trebuie sa ia, le iau pe toate
;      [else
;       (append
;        (k-subsets-raw (union (get-left set) (get-right set) ) k) ; nu iau numarul curent
;        (map (λ (x) (cons (get-value set) x) )
;             (k-subsets-raw (union (get-left set) (get-right set) ) (- k 1) ) ))]))) ; fac unuion, adica restul numerelor in fara de cel curent, iau numarul curent

(define subsets-raw
  (λ (set res-sets)
    (if (null? set)
       res-sets
        (subsets-raw (cdr set) (append res-sets (map (λ (x) (cons (car set) x) ) res-sets ))))))     
      

(define sort-list
  (λ (L)
    (sort L (λ (x y) (< x y) ) ) ))

       
(define k-subsets
  (λ (set k)
     (map sort-list (filter (λ (L) (equal? (length L) k)) (subsets-raw (inorder set) '(()) )) )))


;;returneaza o lista cu liste, fiecare cu x inserat intr.o alta pozitie
(define insert-list
  ( λ (x L)
     (if (null? L)
         (list (list x) )
         (cons
          (cons x L)
          (map (λ (l) (cons (car L) l) ) (insert-list x (cdr L))))) ))

(define generate-perm
  (λ (set)
    (if (null? set)
        '(())
        (foldl append '()
               (map (λ (L)  (insert-list (car set) L) ) (generate-perm (cdr set) ))))))



(define check-zig-zag-comp?
  (λ (set comp)
    (if (equal? (length set) 1)
        #t
        (and 
         (if comp
             (< (car set) (cadr set) )
             (> (car set) (cadr set) ) )
         (check-zig-zag-comp? (cdr set) (not comp) ) ) )) )

(define check-zig-zag?
  (λ (set)
    (or (check-zig-zag-comp? set #t) (check-zig-zag-comp? set #f) ) ))


(define zig-zag-subsets
  (λ (set) 
    (filter (λ (L) (check-zig-zag? L) ) (generate-perm (inorder set) ) ) ))


;;BONUS

(define prioritate
  ( λ (x)
     (if (or (equal? x '+) (equal? x '-) ) 1 2)))

(define make-lists
  ( λ (L)
     (if (equal? (length L) 3)
         (let ( [first (car L)] [second (cadr L)] [third (caddr L)] )
           (if (number? first)
               (list first second (if (number? third) third (make-lists third) ) )
               (list (make-lists first) second  (if (number? third) third (make-lists third) ) ) ))
         (let ([first (car L)] [second (cadr L)] [third (caddr L)] [fourth (cadddr L)] [fifth (cadddr (cdr L) )]
                                    [rest (if (> (length L) 5) (cdddr (cddr L) ) null)] )
           (if (< (prioritate second) (prioritate fourth) )
               (make-lists (append (list first second (list third fourth fifth) ) rest ) )
               (make-lists (append (list (list first second third) fourth fifth) rest) )  ) ))))

(define parser
  (λ (expression)
    (universal-parser (make-lists expression) ) ) )

(define universal-parser
  (λ (expression)
    (let([left (car expression)] [middle (cadr expression)] [right (caddr expression)])
      (if (number? left)
          (make-node (init-node left) (if (number? right) (init-node right) (parser right) ) middle )
          (make-node (parser left) (if (number? right) (init-node right) (parser right) ) middle ) ) )))


(define mini-eval
  (λ (left operator right)
    (cond
      [(equal? '+ operator) (+ left right)]
      [(equal? '- operator) (- left right)]
      [(equal? '* operator) (* left right)]
      [(equal? '/ operator) (/ left right)]) ))

(define evaluate
  (λ (expr-tree)
    (let ( [left (get-left expr-tree)] [right (get-right expr-tree)] [value (get-value expr-tree)] )
      (if (is-leaf? expr-tree)
          value
          (mini-eval (evaluate left) value (evaluate right)))) ) )

;; SECȚIUNE DE TESTARE - NU modificați această linie!
;; ATENȚIE! Pentru a primi punctaj pe temă, NU modificați această secțiune!
;;
;; CHECK - TASK 0 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Task 0 : 30 puncte) ;;check-exp
(define functions (list is-node? is-leaf? is-empty? get-value make-node get-right get-left inorder height insert empty-tree)) ;;check-exp
(define tree0 binary-search-tree) ;;check-exp
(check-exp-part 'is-node .037 (is-node? tree0) #t)
(check-exp-part 'is-leaf?1 .037 (is-leaf? tree0) #f)
(check-exp-part 'is-leaf?2 .037 (is-leaf? (init-node 8)) #t)
(check-exp-part 'is-empty?1 .037 (is-empty? tree0) #f)
(check-exp-part 'is-empty?2 .037 (is-empty? empty-tree) #t)
(check-exp-part 'get-value1 .037 (get-value tree0) 9)
(check-exp-part 'get-value2 .037 (get-value (get-left tree0)) 3)
(check-exp-part 'get-value3 .037 (get-value (get-right tree0)) 12)
(check-exp-part 'make-node .037 (make-node (get-left tree0) (get-right tree0) (get-value tree0)) binary-search-tree)
(check-exp-part 'minimum .0833 (minimum tree0) 1)
(check-exp-part 'maximum .0833 (maximum tree0) 21)
(check-exp-part 'height1 .0833 (height tree0) 5)
(check-exp-part 'height2 .0833 (height (get-left (get-left tree0))) 2)
(check-exp-part 'successor1 .055 (successor tree0 9) 11)
(check-exp-part 'successor2 .055 (successor tree0 5) 6)
(check-exp-part 'successor3 .055 (successor tree0 8) 9)
(check-exp-part 'predecessor1 .056 (predecessor tree0 9) 8)
(check-exp-part 'predecessor2 .056 (predecessor tree0 5) 3)
(check-exp-part 'predecessor3 .057 (predecessor tree0 12) 11)
;; SFÂRȘIT CHECK - TASK 0 - NU modificați această linie!
;;
;; CHECK - Task1 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune!
(Task 1 : 50 puncte) ;;check-exp
(define A (create-tree '(8 9 10 15 8 5 0 1 4 5 9 7 1 0 151 651 61 45 416 2542 -8 3541 644 2 4 8542 51 142 215) functions)) ;;check-exp
(define B (create-tree '(942 4 54 64 94 25 0 -815 485 251 64 8 10 5 4 644 2 216 2541 5 8 7 5254 2542 214 4511) functions)) ;;check-exp
(define C (create-tree '(8 5 4 1 846 54 0 -5552 4 5 810 42 545 842 54 5488 8755 14 679 25 78 25 955 7891 789 8891 97 54 15 2465 155) functions)) ;;check-exp
(define D (create-tree '(8 9 1 5 9 7 5 9 78 1 5 6 9 89 24 52 95 22 94 6 485 18 6 97 8 100 4 9 655 478 92) functions)) ;;check-exp
(check-exp-part 'check-set1 .04 (test-task1 (create-tree '(8 4 2 1 -5 6 1 8 9 5 3 11 17 10 -6 4 8) functions) functions) result-check-set1)
(check-exp-part 'check-set2 .04 (test-task1 (create-tree '(-9 8 2 1 4 0 9 3 4 2 5 9 11 481 51 35 15 0 4 15 251 6551 12 3 4 7 9) functions) functions) result-check-set2)
(check-exp-part 'check-set3 .04 (test-task1 A functions) result-check-set3)
(check-exp-part 'check-set4 .04 (test-task1 B functions) result-check-set4)
(check-exp-part 'check-set5 .04 (test-task1 C functions) result-check-set5)
(check-exp-part 'union1 .005 (test-task1 (union A B) functions) result-union1)
(check-exp-part 'union2 .005 (test-task1 (union C D) functions) result-union2)
(check-exp-part 'union3 .005 (test-task1 (union A D) functions) result-union3)
(check-exp-part 'union4 .005 (test-task1 (union (union A B) (union C D)) functions) result-union4)
(check-exp-part 'intersection1 .01 (test-task1 (intersection A B) functions) result-intersection1)
(check-exp-part 'intersection2 .01 (test-task1 (intersection B C) functions) result-intersection2)
(check-exp-part 'intersection3 .01 (test-task1 (intersection C D) functions) result-intersection3)
(check-exp-part 'intersection4 .01 (test-task1 (intersection (intersection A B) (intersection  C D)) functions) result-intersection4)
(check-exp-part 'complements1 .01 (test-task1 (complements A B) functions) result-complements1)
(check-exp-part 'complements2 .01 (test-task1 (complements C D) functions) result-complements2)
(check-exp-part 'complements3 .01 (test-task1 (complements C D) functions) result-complements3)
(check-exp-part 'complements4 .01 (test-task1 (complements (complements A B) (complements C D)) functions) result-complements4)
(check-exp-part 'insert1 .005 (test-task1 (insert B -7) functions) result-insert1)
(check-exp-part 'insert2 .005 (test-task1 (insert A 59525) functions) result-insert2)
(check-exp-part 'insert3 .005 (test-task1 (insert C 988522) functions) result-insert3)
(check-exp-part 'insert4 .005 (test-task1 (insert D -812612) functions) result-insert4)
(check-exp-part 'remove1 .02 (test-task1 (remove binary-search-tree (minimum binary-search-tree)) functions) result-remove1)
(check-exp-part 'remove2 .02 (test-task1 (remove binary-search-tree 9) functions) result-remove2)
(check-exp-part 'remove3 .02 (test-task1 (remove binary-search-tree 3) functions) result-remove3)
(check-exp-part 'remove4 .02 (test-task1 (remove (remove (remove A (successor A 10)) (predecessor A 0)) 416) functions) result-remove4)
(check-exp-part 'complex1 .02 (test-task1 (union A (intersection B C)) functions) result-complex1)
(check-exp-part 'complex2 .02 (test-task1 (insert (intersection (complements B C) (remove (union A B) (predecessor A 51))) 7851) functions) result-complex2)
(check-exp-part 'complex3 .02 (test-task1 (insert (remove (remove (union (intersection (complements B C) (complements B A)) binary-search-tree) 214) 1) 1) functions) result-complex3)
(check-exp-part 'complex4 .02 (test-task1 (union (intersection (complements B A) (union C D)) (complements A D)) functions) result-complex4)
(check-exp-part 'complex5 .02 (test-task1 (intersection (union (complements A B) (complements C D)) (complements (intersection A B) (intersection C D))) functions) result-complex5)
(check-exp-part 'complex6 .02 (test-task1 (remove (insert (union (union (complements A B) (intersection C D)) (intersection (complements C D) (intersection A B))) 22) -8) functions) result-complex6)
(check-exp-part 'complex7 .02 (test-task1 (union (union (intersection A C) (complements A D)) (intersection (complements B C) (intersection B D))) functions) result-complex7)
(check-exp-part 'complex8 .02 (test-task1 (union (union (union A B) (union C D)) (intersection (intersection A B) (intersection C D))) functions) result-complex8)
(check-exp-part 'complex9 .02 (test-task1 (intersection (union (complements A B) (complements B A)) (intersection (union A B) (union C D))) functions) result-complex9)
(check-exp-part 'complex10 .02 (test-task1 (insert (remove (intersection (union (complements B A) (union (complements C D) (intersection A B))) (intersection (complements B (union A C)) (union C D))) 485) 100) functions) result-complex10)
(check-exp-part 'height-balanced1 .04 (check-self-balancing-tree B functions) #t)
(check-exp-part 'height-balanced2 .04 (check-self-balancing-tree A functions) #t)
(check-exp-part 'height-balanced3 .04 (check-self-balancing-tree C functions) #t)
(check-exp-part 'height-balanced4 .04 (check-self-balancing-tree D functions) #t)
(check-exp-part 'height-balanced5 .04 (let [(tree (create-tree '(1 2 3 4 5 6 7 8 9 10) functions))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced6 .04 (let [(tree (create-tree '(20 19 18 17 16 15 10 9 8 7 6 5 4 3 2 1) functions))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced7 .04 (let [(tree (union A (intersection B C)))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced8 .04 (let [(tree (remove (insert (union (complements A D) (intersection B C)) 24) 416))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced9 .04 (let [(tree (union (remove binary-search-tree 9) A))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced10 .04 (let [(tree (intersection (union (remove A (get-value A)) (remove B (get-value B))) (remove C (get-value C))))] (check-self-balancing-tree tree functions)) #t)
;; SFÂRȘIT CHECK - TASK 1 - NU modificați această linie!
;;
;; CHECK - TASK 2 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Task 2 : 20 puncte) ;;check-exp
(check-exp-part 'k-subsets1 0.1 (test-subsets (k-subsets (intersection A B) 8) result-k-subsets1) #t)
(check-exp-part 'k-subsets2 0.1 (let [(subsets (k-subsets binary-search-tree 11))] (and (= (length subsets) 78) (not (equal? (member '(2 3 5 6 8 9 11 12 13 15 21) subsets) #f)))) #t)
(check-exp-part 'k-subsets3 0.1 (test-subsets (k-subsets (create-tree '(1 2 3 4 5) functions) 3) result-k-subsets3) #t)
(check-exp-part 'k-subsets4 0.1 (test-subsets (k-subsets (create-tree '(8 7 6 5) functions) 2) result-k-subsets4) #t)
(check-exp-part 'k-subsets5 0.1 (test-subsets (k-subsets D 20) result-k-subsets5) #t)
(check-exp-part 'zig-zag-subsets1 0.1 (test-subsets (zig-zag-subsets (create-tree '(1 2 3 4 5 6) functions)) result-zig-zag1) #t)
(check-exp-part 'zig-zag-subsets2 0.1 (test-subsets (zig-zag-subsets (create-tree '(1 2 3 4) functions)) result-zig-zag2) #t)
(check-exp-part 'zig-zag-subsets3 0.1 (test-subsets (zig-zag-subsets (create-tree '(1 7 9 10 5) functions)) result-zig-zag3) #t)
(check-exp-part 'zig-zag-subsets4 0.1 (test-subsets (zig-zag-subsets (create-tree '(98 5 1 -85 -457) functions)) result-zig-zag4) #t)
(check-exp-part 'zig-zag-subsets5 0.1 (length (zig-zag-subsets (create-tree '(982 616 542 125 98 85) functions))) 122)
;; SFÂRȘIT CHECK - TASK 2 - NU modificați această linie!
;;
;; CHECK - BONUS - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Bonus 3 : 20 puncte BONUS) ;;check-exp
(check-exp-part 'bonus1 0.1 (test-bonus (parser '(1 + (((2 * 3) - 4) * 5))) functions) 11)
(check-exp-part 'bonus2 0.1 (test-bonus (parser '((((5 + 8) * (9 - (8 / 2))) + (8 * 9)) * 10)) functions) 1370)
(check-exp-part 'bonus3 0.1 (test-bonus (parser '((5 * 8) - (7 * (3 + (5 * (10 / 2)))))) functions) -156)
(check-exp-part 'bonus4 0.1 (test-bonus (parser '(((((80 - 78) + 15) * 4 ) / 2) + (7 + (((5 * 3) - 2) * 4)))) functions) 93)
(check-exp-part 'bonus5 0.2 (test-bonus (parser '(((((((((5 + 8) + (9 + 8)) * 3) + (8 - 7)) * 2) + 10) / 2) * 10) - (5 + (7 + (8 * (1 + 2)))))) functions) 924)
(check-exp-part 'bonus6 0.2 (test-bonus (parser '((((((5 + 6) * 7) + 9) * 10) / 2) + (7 * (2 * (4 * (10 - (7 + (1 * (2 - 1))))))))) functions) 542)
(check-exp-part 'bonus7 0.2 (test-bonus (parser '(((5 + (7 - (2 * (3 + (9 - (7 + (8 + (5 * 2)))))))) + (5 * (((2 + 2) * (3 + 7)) + (7 * (9 - (4 + 7)))))) / 2)) functions) 84)
;; SFÂRȘIT CHECK - BONUS - NU modificați această linie!
;; SFÂRȘIT SECȚIUNE DE TESTARE

(sumar)
