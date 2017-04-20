#lang racket

; ignorați următoarele linii de cod.
(define show-defaults 2) ; câte exerciții la care s-au întors rezultate default să fie arătate detaliat
(define prepend #f) (define name-ex '(exercițiul . exercițiile)) ; variante: '(exercițiul . exercițiile) sau '(testul . testele) sau '(task . taskurile)
(define default-results '(#f 0 () your-code-here)) ; ce rezultate default sunt întoarse în exerciții
(define : 'separator) (define punct 'string) (define puncte 'string) (define BONUS 'string) (define total 0) (define all '()) (define n-ex 0) (define p-ex 0) (define defaults '())
(define (ex n sep p . s) (set! n-ex n) (set! p-ex p) (set! all (cons (list n p) all))) (define exercițiul ex) (define (p L) (map (λ (e) (display e) (display " ")) L) (newline))
(define (check-exp given expected) (check-exp-part "" 1 given expected)) (define (check-exp-part part percent given expected) (if (not (equal? given expected)) (and
  (when (member given default-results) (set! defaults (cons (if (< percent 1) (cons n-ex part) n-ex) defaults))) (when (or (not (member given default-results))
        (<= (length defaults) show-defaults)) (p `(NU: la ,(car name-ex) ,(if (< percent 1) (cons n-ex part) n-ex) rezultatul ,given diferă de cel așteptat: ,expected))))
 (let ((pts (* p-ex percent))) (and (if prepend (printf "+~v: " pts) (printf "OK: "))
  (p `(,(car name-ex) ,(if (< percent 1) (cons n-ex part) n-ex) rezolvat: + ,pts ,(if (= pts 1) 'punct 'puncte))) (set! total (+ total pts))))))
(define (sumar) (when (and (not (null? defaults)) (< show-defaults (length defaults))) (p `(... rezultatul implicit dat la ,(cdr name-ex) ,(reverse defaults)))) (p `(total: ,total puncte)))
(define (mark-helper) (printf "----------~nEx  puncte    Total până aici~n") (foldr (λ (e-p t) (p `(,(car e-p) : ,(cadr e-p) puncte. total 1 - ,(car e-p) : ,(+ t (cadr e-p)))) (+ t (cadr e-p))) 0 all))

;; Ne propunem să realizăm o aplicație care să certifice stăpânirea următoarelor concepte/competențe:
;; - abstractizarea la nivel de date (tipuri de date abstracte care oferă o interfață (constructori + operatori)
;;   utilizatorului, astfel încat acesta să nu fie nevoit să cunoască implementarea internă)
;; - abstractizarea la nivel de proces (abilitatea de a folosi funcționale)
;; - caracteristica funcțiilor de a fi valori de ordinul întâi
;; - abilitatea de a implementa un program mai mare ca o serie de funcții care se apelează unele pe altele
;; - folosirea expresiilor de legare statică a variabilelor (let, let*, letrec, named let)

;; Pentru aceasta, definim structura de date graf orientat.

;; Reținem un graf ca pe o listă de noduri și o listă de arce:
(define (make-directed-graph V E) (cons V E))
(define G1 (make-directed-graph
            '(2 3 5 7 8 9 10 11)
            '((3 8) (3 10) (5 11) (7 8) (7 11) (8 9) (11 2) (11 9) (11 10))))
(define G2 (make-directed-graph 
            '(m n o p q r s t u v w x y z) 
            '((m q) (m r) (m x) (n o) (n q) (n u) (o r) (o s) (o v) (p o) (p s) (p z) (q t) (r u) (r y) (s r) (u t) (v w) (v x) (w z) (y v))))


(exercițiul 1 : 3 puncte)
;; Completați tipul de date graf orientat cu următorii operatori:

;; întoarce lista de noduri din graf (0,25p)
(define (get-nodes G)
  (if (null? G)
      '() 
       (car G) ) )

(check-exp-part 'a (/ 1. 12) (get-nodes G2) '(m n o p q r s t u v w x y z))

;; întoarce lista de arce din graf (0,25p)
(define (get-edges G)
  (if (null? G)
      '()
      (cdr G) ) )
  

(check-exp-part 'b (/ 1. 12) (get-edges G1) '((3 8) (3 10) (5 11) (7 8) (7 11) (8 9) (11 2) (11 9) (11 10)))

;; lista nodurilor către care nodul v are arc (1.25p)
;; o veți implementa obligatoriu folosind funcționale și/sau funcții anonime, fără recursivitate

(define (outgoing G v)
   (map cadr (filter (λ (x)  (equal? (car x) v)  ) (get-edges G) )
  ) ) 


;(cons (assoc v (get-edges G) ) (outgoing 
;;(assoc 3 (get-edges G1) )

        
(check-exp-part 'c (/ 5. 12) (outgoing G2 'o) '(r s v))

;; lista nodurilor care au arc către nodul v (1.25p)
;; o veți implementa obligatoriu folosind funcționale și/sau funcții anonime, fără recursivitate
(define (incoming G v)
    (map car (filter (λ (x) (equal? v (cadr x) ) ) (get-edges G) ) ) )
  ; (filter ( λ (x) (equal? (cdr x) v)) (get-edges G) ) )


(check-exp-part 'd (/ 5. 12) (incoming G1 9) '(8 11))

(exercițiul 2 : 7 puncte)
;; Realizați parcurgerea DFS a grafului G pornind dintr-un nod dat v. (4p)
;; Rezultatul final va fi o listă ordonată de perechi cu punct de forma (nod-vizitat . parinte)
;; Folosiți let-uri și/sau funcționale.


;(define helper
;  (lambda (graph stack visited)
;    (if (empty? stack)
;      '() ;we're done. you've got a nice list of visited nodes now, what will you do with it? Return it?
;      (let exploreaza-nod([currentNode (car stack)] [vecini outgoing (car stack)])
;(if (member? currentNode visited)
;             (helper graph (cdr stack) visited)
;             (if (not (member? (car vecini) visited)  )
;                 (helper graph (cons (car vecini) stack) visited) ) ) ) ) ) ) 
(define  (obtine-perechi x L) 
     (if (null? L)
         '()
         (cons (cons  (car L) x ) (obtine-perechi x (cdr L) ) ) ) )

(define (member? x L)
  (if (null? L)
      #f
      (or (equal? x (car L) ) (member? x (cdr L) ) ) ) )
(member? 8 '(1 2 3 4) )


(obtine-perechi 7 '(1 2 3 4) )
(list (obtine-perechi 7 '(1 2 3 4) ) (cdr (list (cons 3 4) (cons 2 3) (cons 8 9) ) ) )
(cdr (list (cons 1  2) (cons 2  5) (cons 4 8) ) )

(define (helper G vizitat devizitat)
  (if (null? devizitat)
      (reverse vizitat)
      (let ( [curentPair (car devizitat) ] )
        (if (not (assoc (car curentPair) vizitat ) )
            ;;daca nu e vizitat iau toti vecinii lui si ii bag in lista devizitat, pe el il bag in lista vizitat;         
            (helper G (cons curentPair vizitat) (append (obtine-perechi (car curentPair) (outgoing G (car curentPair) ) ) (cdr devizitat) ) )
            ;daca e vizitat:
            (helper G vizitat (cdr devizitat) ) ) ) ) )
(define (dfs-one G v) 
   (helper G '() (list (cons v  'none) ) ) ) 

(check-exp-part 'a (/ 2. 7) (dfs-one G1 7) '((7 . none) (8 . 7) (9 . 8) (11 . 7) (2 . 11) (10 . 11)))
(check-exp-part 'b (/ 2. 7) (dfs-one G2 'n) '((n . none) (o . n) (r . o) (u . r) (t . u) (y . r) (v . y) (w . v) (z . w) (x . v) (s . o) (q . n)))

;; Realizați parcurgerea DFS completă a grafului G. (3p)
;; Rezultatul final va fi o listă ordonată de liste ca la exercițiul anterior.
;; Se realizează parcurgerea DFS din primul nod al grafului, rezultând prima listă din lista rezultat.
;; Dacă au ramas noduri nevizitate, se continuă cu o nouă parcurgere din primul nod nevizitat găsit.
;; Folosiți let-uri. Folosiți funcții auxiliare, dacă aveți nevoie.

(define (scoate-noduri L) 
  (if (null? L)
      '()
      (cons  (car (car L) ) (scoate-noduri (cdr L) ) ) ) )
  
(define (dfs G)
  (if (null? (get-nodes G) )
      '()
      (let ( [vizitate (helper G '() (list (cons (car (get-nodes G) ) 'none ) ) )] ) 
        (cons vizitate (dfs 
                          (cons 
                           (filter (λ (x) (if (not (member x (scoate-noduri vizitate) ) ) #t #f ) ) (get-nodes G) )
                           (filter (λ (x) (if (not (member (cadr x)  (scoate-noduri vizitate) ) )  #t #f ) ) (get-edges G) ) ) ) ) ) ) )
      

(check-exp-part 'c (/ 3. 14) (dfs G1) '(((2 . none)) ((3 . none) (8 . 3) (9 . 8) (10 . 3)) ((5 . none) (11 . 5)) ((7 . none))))
(check-exp-part 'd (/ 3. 14) (dfs G2) '(((m . none) (q . m) (t . q) (r . m) (u . r) (y . r) (v . y) (w . v) (z . w) (x . v)) ((n . none) (o . n) (s . o)) ((p . none))))


(exercițiul 3 : 4 puncte)
;; Sortați topologic nodurile dintr-o listă de tipul listelor generate de funcția dfs-one, după metoda indicată mai jos.
;; Graful se presupune aciclic.
;; Mai întâi se adaugă în rezultat primul nod parcurs căruia i s-au explorat deja toți fiii (dacă are).
;; Al doilea astfel de nod este adăugat în fața acestuia etc.
;; exemplu: lista ((m . none) (q . m) (t . q) (r . m) (u . r) (y . r) (v . y) (w . v) (z . w) (x . v))
;; se poate rescrie în notație cu paranteze ca (m (q (t t) q) (r (u u) (y (v (w (z z) w) (x x) v) y) r) m)
;; fiecare nod apare ca prim element al listei care îi conține și fiii
;; fiii lui m sunt q și r, fiii lui r sunt u și y etc (puteți decomenta lista de mai sus ca sa vedeți cum se împerechează parantezele)
;; primul nod terminat este t (prima paranteza închisa), urmează q, u, z, w, x, v, y, r, m
;; deci sortarea topologica este (m r y v x w z u q t)
;; observație: dacă aveți o serie de noduri "deschise" la un moment dat, ele nu se pot închide altfel decât în ordine inversă celei în care au fost deschise
(define (top-sort-one dfs-list)
  'your-code-here)

(check-exp-part 'a .5 (top-sort-one '((3 . none) (8 . 3) (9 . 8) (10 . 3))) '(3 10 8 9))
(check-exp-part 'b .5 (top-sort-one '((m . none) (q . m) (t . q) (r . m) (u . r) (y . r) (v . y) (w . v) (z . w) (x . v))) '(m r y v x w z u q t))

(exercițiul 4 : 1 punct)
;; Sortați topologic nodurile grafului G - folosind funcționale, fără recursivitate.
;; Graful se presupune aciclic.
(define (top-sort G)
  'your-code-here)

(check-exp-part 'a .5 (top-sort G1) '(7 5 11 3 10 8 9 2))
(check-exp-part 'b .5 (top-sort G2) '(p n o s m r y v x w z u q t))

(sumar)
