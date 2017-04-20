#lang racket
(require test-engine/racket-tests)

;; Avem de rezolvat urmatoarea problema:
;; Sa se gaseasca toate numerele naturale, mai mici sau egale cu un numar n,
;; care sunt palindroame atat in baza 10 cat si in baza 2

;; Pentru a rezolva problema, vom defini, pe rand, functii pentru:
;; - reprezentarea (ca lista de "cifre") a unui numar natural intr-o baza b
;; - testul ca o lista este palindrom (adica este totuna cu lista inversata)
;; - testul ca un numar este palindrom in toate bazele dintr-o lista data
;; - parcurgerea tuturor numerelor pana la un numar dat n, si selectarea celor
;;   palindroame in toate bazele dintr-o lista data de baze

;; 1. (2p)
;; Fie urmatoarele axiome pentru obtinerea reprezentarii unui numar natural
;; in baza b.
;; num->base(0,b) = [ ]                                   ; pt n=0
;; num->base(n,b) = num->base(n div b, b) ++ [ n mod b ]  ; pt n>0
;; Implementati functia corespunzatoare in Scheme:

(define (num->base n b)
  (if ( = n 0)
      '()
         (append  (num->base (quotient n b) b )   (list (modulo n b) ) ) ) ) 
      

(check-expect (num->base 489 10) '(4 8 9))
(check-expect (num->base 489 2) '(1 1 1 1 0 1 0 0 1))

;; 2. (2p)
;; Fie urmatoarele axiome pentru inversarea unei liste.
;; rev([ ]) = [ ]
;; rev(x:l) = rev(l) ++ [x]
;; Implementati functia corespunzatoare in Scheme:

(define (rev L)
  (if  (null? L) 
      '()
    ( append (rev (cdr L) ) (list (car L) ) ) ) )

(check-expect (rev '(5 1 4 8 7)) '(7 8 4 1 5))

;; 3. (1p)
;; Implementati testul ca o lista L este palindrom.

(define (palindrome? L)
  (if (equal? L (rev L) ) #t #f ) ) 

(check-expect (palindrome? '(1 4 4 1)) #t)
(check-expect (palindrome? '(1 4 2 4 1)) #t)
(check-expect (palindrome? '(1 4 4 1 4 1)) #f)

;; 4. (0.5p)
;; Verificati daca se poate obtine o lista palindrom din apendarea a 2 liste sau a inverselor lor.
;; l1 ++ l2, rev(l1) ++ rev(l2).
(define (append-palindrome? L1 L2)
  (if (or ( palindrome? (append L1 L2 ) )  (palindrome? (append  (rev L1)    (rev L2)   ) ) ) #t #f ) )

(check-expect (append-palindrome? '(4 1) '(1 4 2)) #t)
(check-expect (append-palindrome? '(1 4) '(1 4 2)) #f)

;; 5. (2,5p)
;; Testati ca n este palindrom in toate bazele din lista Bases

(define (all-palindromes? n Bases)
  (if (null? Bases) #t
      (and (palindrome? (num->base n (car Bases) ) ) (all-palindromes? n (cdr Bases) ) )
     )
  )

(check-expect (all-palindromes? 585 '(2 10)) #t)
(check-expect (all-palindromes? 594 '(2 10)) #f)

;; 6. (2,5p)
;; Gasiti toate numerele naturale, mai mici sau egale cu n, care sunt
;; palindroame in toate bazele din lista Bases

(define (palindromes-to-n n Bases)
  (if (= n -1)
      '()
      (if (equal? (all-palindromes? n Bases) #t ) (append  (palindromes-to-n (- n 1) Bases) (list n) )
          (palindromes-to-n (- n 1) Bases ) ) )
  )

(check-expect (palindromes-to-n 100 '(2 10)) '(0 1 3 5 7 9 33 99))
;; 7. BONUS? (2p)
;; L este o lista de liste.
;; Gasiti toate perechile de liste din lista L,
;; astfel incat concatenate formeaza o lista palindrom.
;; Hint: ex 4

(define (findPalindromes L1 L2)
  (if (null? L2)
      '()
      (append (findPalindromes L1 (cdr L2) )  (if (append-palindrome? L1 (car L2) )  (list (cons L1 (list (car L2) ) ) )  '() ) )  
  )
)

(define (pairs-palindromes L)
  (if (null? L) 
     '()
      (append (findPalindromes (car L) (cdr L) ) (pairs-palindromes (cdr L) ) )
   )
)


(check-expect (pairs-palindromes '((0) (1 3) (3) (5) (7 9) (33 3) (99))) '(((1 3) (3)) ((3) (33 3))))
(check-expect (pairs-palindromes '((0 1) (0 1) (3) (9) (33) (99))) '())

;; 8. BONUS (3p)
;; Un numar Lychrel este un numar natural care nu devine palindrom in urma
;; procesului iterativ de a aduna numarul cu inversul sau.
;; Exemple de numere care nu sunt Lychrel:
;; 56 devine palindrom dupa o iteratie: 56 + 65 = 121
;; 59 devine palindrom dupa 3 iteratii:
;; 59 + 95 = 154, 154 + 451 = 605, 605 + 506 = 1111
;; Sa se gaseasca numerele naturale pana la Max care nu sunt si nu devin
;; palindroame dupa n iteratii

(define (pow_ten x)
  (if (= x 0)
      1
      (* 10 (pow_ten (- x 1) ) ) )
  )


(pow_ten 3)

(define (cate_cifre x)
  (if (= x 0)
      -1
      (+ 1 (cate_cifre (quotient x 10 ) ) )
      )
  )

(cate_cifre 12423)



(define (inverse nr iter) 
  (if (< nr 10 ) nr
       (+ (* (pow_ten iter) (modulo nr 10 ) ) (inverse (quotient nr 10 ) (- iter 1) ) )  
  ))

(inverse 523423 (cate_cifre 523423) )




(define (is-lychrel? nr n) 
  (if (= n -1 ) #f
  ( or (  palindrome? (num->base  (+ nr (inverse nr (cate_cifre nr) ) ) 10 ) )   (is-lychrel? (+ nr (inverse nr (cate_cifre nr) ) ) (- n 1) )  )
  ) )



(define (maybe-lychrel Max n)
  (if (= Max -1) '() 
      (append (maybe-lychrel (- Max 1) n )   (if  (is-lychrel? Max n ) '() (list Max) ) ) ) ) 

(check-expect (maybe-lychrel 200 25) '(196))

(test)