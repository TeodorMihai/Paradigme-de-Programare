qd{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.List
import Debug.Trace
import TestPP

{-
Expresia `undefined` are orice tip dar nu poate fi evaluată.
-}

{-
1. (1p)
Construiți funcții simple pentru următoarele tipuri (completați definițiile):
-}
identity :: a -> a
identity a = a

isZero :: Int -> Bool
isZero 0 = True
isZero _ = False

succesor :: Int -> Int
succesor x = x + 1

listsToPair :: [a] -> [b] -> ([a], [b])
listsToPair l1 l2 = (l1 ,l2)

-- Verificare: check1
check1 :: TestPP ()
check1 = do
  assertVal "[1] listsToPair [isZero 3] undefined" 0.25 $ -- 0.25p
    not $ head $ fst $ listsToPair [isZero 3] undefined
  assertVal "[1] identity [isZero 0]" 0.25 $ -- 0.25p
    head $ identity $ [isZero 0]
  assertVal "[1] listsToPair [isZero 0] [isZero (succesor 0)]" 0.25 $ -- 0.25p
    let res = listsToPair [isZero 0] [isZero (succesor 0)] in (head (fst res)) && (not (head (snd res)))
  assertVal "[1] listsToPair [1, 2, 3] undefined" 0.25 $ -- 0.25p
    fst (listsToPair [1..3] undefined) == [1..3]

{-
2. (1p)
Implementați funcția `unzip2`
-}
unzip2  :: [(a, b)] -> ([a], [b]) 

unzip2 [] = ( [] , [] ) 
unzip2 x =  ( fst (head x) : l1 ,  snd (head x)  : l2 ) where (l1, l2) = unzip2 (tail x)

-- Verificare: check2
check2 :: TestPP ()
check2 = do
  assertVal "[2] unzip2 (zip)" 1 $ -- 1p
    unzip2 (zip [1,2,3] ["a","b","c"]) == ([1,2,3], ["a","b","c"])

{-
3. (1p)
Implementați, folosind obligatoriu list-comprehensions, lista tuturor numerelor prime până la n.
-}

divizori x = [ i | i <- [2 .. (x - 1) ] , x `mod` i == 0 ]

--ispirme? x 1 = False
isprime x = if divizori x == [] then True else False


primes :: Int -> [Int]
primes n = [ x | x <- [2 .. n], isprime x]

-- Verificare: check3
check3 :: TestPP ()
check3 = do
  assertVal "[3] primes 30" 1 $ -- 1p
    primes 30 == [2,3,5,7,11,13,17,19,23,29]

{-
4. (3p)
Implementați, folosind obligatoriu list-comprehensions, operații pe mulțimi:
intersecție, diferență, produs cartezian. Utilizați ulterior funcțiile definite anterior
pentru a reprezenta reuniunea mulțimilor.
-}
setIntersection :: Eq a => [a] -> [a] -> [a]
setIntersection a b = [ x |  y <- a , x  <- b , (y == x)]

setDiff :: Eq a => [a] -> [a] -> [a]
setDiff a b = [x | x <- a , not (x `elem` b)]

cartProduct :: [a] -> [b] -> [(a, b)]
cartProduct a b = [ (x , y) |  x <- a , y <- b ]

setUnion :: Eq a => [a] -> [a] -> [a]
setUnion a b = [ x | x <- setIntersection a b ] ++ [y | y <- setDiff a b] ++ [ z | z <- setDiff b a]

-- Verificare: check4
check4 :: TestPP ()
check4 = do
  assertVal "[4] cartProduct" 0.5 $ -- 0.5p
    cartProduct [1, 2] [3, 4, 5] == [(1, 3), (1, 4), (1, 5), (2, 3), (2, 4), (2, 5)]
  let a = [1, 7, 3, 6, 2]
      b = [2, 8, 6, 10, 4, 1]
  assertVal "[4] setIntersection" 0.75 $ -- 0.75p
    sort (setIntersection a b) == [1, 2, 6]
  assertVal "[4] setDiff" 0.75 $ -- 0.75p
    sort (setDiff a b) == [3, 7]
  assertVal "[4] setUnion" 1 $ -- 1p
    sort (setUnion a b) == [1, 2, 3, 4, 6, 7, 8, 10]

{-
5. (2p)
Implementați o funcție ce calculează cmmdc-ul unei liste de numere pozitive.
Lista va avea lungime minima 2.
-}

getcmmdc x y = if y == 0 then x else getcmmdc y   ( x `mod` y )   

cmmdc :: [Integer] -> Integer

cmmdc l 
  | length l == 2  = getcmmdc (head l) (head (tail l))
  | otherwise = ( getcmmdc (head l) x ) where x = cmmdc ( tail l )

-- Verificare: check5
check5 :: TestPP ()
check5 = do
  assertVal "[5] cmmdc" 1 $ -- 1p
    cmmdc [6, 24, 18, 33, 99] == 3
  assertVal "[5] cmmdc x y" 1 $ -- 1p
    (\l -> let c = cmmdc l in and (map (\x -> x `mod` c == 0) l)) [4, 5, 8, 21]

{-
6. (1p)
Verificaţi dacă o propoziţie este palindrom. O propoziţie este palindrom dacă conţine cuvintele 
în aceeaşi ordine, fie ea citită de la început spre sfârşit sau de la sfârşit spre început.
Exemplu: Ana este ingenioasa si ingenioasa este Ana -> True

Hint: s-ar putea să aveți nevoie de funcții auxiliare. Puteți folosi `words`
pentru a obține cuvintele din frază și `unwords` sau `++` pentru a obține
frază din cuvinte.
-}

palindromen s
  | length s < 2 = True
  | otherwise = if ( head s ) == ( last s ) then  palindromen ( tail (init s ) )  else False

palindrome :: String -> Bool
palindrome s = palindromen (words s)

-- Verificare: check6
check6 = do 
  let prop = "Ana este ingenioasa si ingenioasa este Ana" in
    assertVal "[6] palindrome" 0.5 $ -- 0.5p
      palindrome prop
  let prop2 = "Ana bea cafea si cafea nu bea Ana" in
    assertVal "[6] not palindrome" 0.5 $ -- 0.5p
      not $ palindrome prop2

{-
7. (2p)
Duplicaţi toate cuvintele dintr-o propoziţie.
Exemplu: Ce laborator frumos! -> Ce Ce laborator laborator frumos! frumos!
Hint: Ar putea fi utile funcţiile concat sau "++" pentru concatenarea cuvintelor.
-}

dupn s 
  | s == [] = []
  | otherwise  =  head s : ( head s : dupn (tail s) )

dup :: String -> String
dup sentence = unwords (dupn (words sentence) )

-- Verificare: check7
check7 = do
  assertVal "[7] dup" 1 $ -- 1p
    dup "Ce laborator frumos!" == "Ce Ce laborator laborator frumos! frumos!"
  assertVal "[7] dup, again" 1 $ -- 1p
    null $ (\sentence -> filter (/= 2) $ map length $ group $ words $ dup sentence) "To be or not to be"

{-
8. (BONUS, 2p)
Găsiţi numărul de apariţii ale fiecărui element dintr-o listă în lista respectivă. 
Rezultatul va fi returnat ca o listă de tupluri, în care primul element al perechii 
va fi elementul din listă, iar al doilea element al perechii va fi numărul de apariţii în listă. 
Cum rezultatul va fi similar unui dicţionar, chiar dacă un element va apărea de mai multe ori în listă, 
va trebui să fie prezent într-o singură pereche în dicţionar.
  
Hint: s-ar putea să aveți nevoie de funcții auxiliare. Puteți folosi `group` pentru
a grupa elementele egale în liste separate şi funcţia sort pentru a sorta o listă. 
-}

nrOccaux l = [(head z , length z) | z <- l] 


nrOcc :: Ord a => [a] -> [(a, Int)]
nrOcc l = nrOccaux (group (sort l))
-- Verificare: check8
check8 =
  assertVal "[8] number of occurrences" 2 $ -- 2p
    nrOcc [1, 2, 3, 4, 2, 3, 3, 1, 2, 3, 3, 4] == [(1, 2), (2, 3), (3, 5), (4, 2)]

{-
9. (BONUS, 2p)
Se dă un poligon reprezentat prin vârfurile sale. 
Să se reprezinte, sub formă de perechi de puncte, toate diagonalele poligonului.
-}

vertiges = ['A', 'B', 'C', 'D', 'E', 'F']

makepairs x l = [( x , y ) | y <- l]

fun l 
  | length l < 3  = [ ]
  | otherwise = ( makepairs (head l) (tail (tail l) ) ) ++ (fun (tail l))

diagonals = setDiff (fun vertiges) [(head vertiges, last vertiges) ]

-- Verificare: check9
check9 =
  let answer = [('A','C'),('A','D'),('A','E'),('B','D'),('B','E'),('B','F'),('C','E'),('C','F'),('D','F')]
  in assertVal "[9] diagonals" 2 $
      diagonals == answer

{-
Helpers for testing :)
-}
runAllTests = runTestPP $
  sequence_[check1, check2, check3, check4, check5, check6, check7, check8, check9]
