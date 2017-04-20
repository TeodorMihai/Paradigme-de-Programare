module MiniCrypto where

{-
  PP, Laboratorul 7

  Laboratorul presupune implementarea unei mini-biblioteci de primitive
  criptografice: cifrări flux, cifrări bazate pe substituție (Caesar și
  Vigenere).
-}

import Data.List
import Data.Word
import Data.Bits
import System.Random hiding (randoms)
import TestPP

{-
  Funcții auxiliare: conversie Char-Word
-}
charToWord :: Char -> Word8
charToWord = fromIntegral . fromEnum

wordToChar :: Word8 -> Char
wordToChar = toEnum . fromIntegral

{-
  1. (1p)
  Construiți funcția myCycle, care ia ca argument o listă și întoarce lista
  repetată la infinit. Ex: myCycle [1,2,3] = [1,2,3,1,2,3,1,2,3,1,2,3,..]

  Hint: Puteți defini funcția „point-free”, folosind funcții din cadrul
  modulului Data.List.
  http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-List.html
-}

check1 :: TestPP ()
check1 = do
  assertVal "[1] myCycle" 1 $ -- 1p
    (take 42 $ myCycle xs) == (take 42 $ cycle xs)
  where
  xs = [1,2,3,4]

myCycle :: [a] -> [a]
myCycle [] = []
myCycle l = cycle l

{-
  2. (2p)
  Construiţi o progresie aritmetică şi o progresie geometrică pornind de la 
  primul termen şi raţia în fiecare dintre cazuri.
  Ex: arithmetic 1 3 = [1,4,7,..]
      geometric  2 3 = [2,6,18,..]

  Hint: folosiţi funcţia iterate din cadrul modulului Data.List.
  http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-List.html
-}

check2 :: TestPP ()
check2 = do
  assertVal "[2] arithmetic" 1 $ -- 1p
    (take 10 $ arithmetic initial r) == [5,11,17,23,29,35,41,47,53,59]
  assertVal "[2] geometric" 1 $ -- 1p
    (take 10 $ geometric initial q) == [5,10,20,40,80,160,320,640,1280,2560]
  where
  initial = 5
  r = 6
  q = 2

arithmetic :: Num a => a -> a -> [a]
arithmetic initial ratio = iterate ( \ x -> x + ratio) initial

geometric :: Num a => a -> a -> [a]
geometric initial ratio = iterate ( \ x -> x * ratio) initial
{-
  2. (2p)
  Construiți o funcție care întoarce un șir infinit de numere pseudo-aleatoare,
  plecând de la o valoare „seed” întreagă. Tipul elementelor listei va fi Word8
  (numerele vor fi între 0 și 255). Folosiți funcțiile definite în modulul
  System.Random pentru a genera numere. Folosiți fromIntegral pentru a realiza
  conversii între tipuri numerice întregi.
  http://www.haskell.org/ghc/docs/6.12.2/html/libraries/random-1.0.0.2/System-Random.html

  Ex: > take 10 $ randoms 42
  [38,166,220,81,67,142,213,118,105,10]

  Hint: Folosiți-vă de mkStdGen, next și (eventual) alte funcții din
  System.Random. *Nu* este necesară folosirea de funcții impure (care întorc
  valori de tipul IO).
-}

check3 :: TestPP ()
check3 = do
  assertVal "[3] randoms" 2 $ -- 2p
    (take 10 $ randoms 42) == [38,166,220,81,67,142,213,118,105,10]

--fromInteger :: Int -> Word8

randoms :: Int -> [Word8]
randoms seed = getNumber ( next ( mkStdGen seed ) )

getNumber (x , gen) = (fromIntegral x ) : ( getNumber ( next gen ) )
{-
  4. (3p)
  Implementați funcția substCrypt, care primește o listă de asocieri
  (caracter-clar, caracter-criptat) și un
  șir de caractere și întoarce șirul de caractere criptat pe baza tabelei.

  Implementați funcția tableToFunc, care primește o listă de asocieri
  (caracter-clar, caracter-criptat) și întoarce o funcție de substituție.

  Observație: substCrypt va fi implementată obligatoriu „point-free” (nu va
  avea parametri expliciți), folosind funcționale și/sau clauze let/where.
-}

check4 :: TestPP ()
check4 = do
  assertVal "[4] substCrypt" 2 $ -- 2p
    substCrypt rot13Table str == cryptstr
  assertVal "[4] tableToFunc" 1 $ -- 1p
    tableToFunc rot13Table 'D' == 'Q'
  where
  str = "THEQUICKBROWNFOXJUMPSOVERTHELAZYDOG"
  cryptstr = "GURDHVPXOEBJASBKWHZCFBIREGURYNMLQBT"

rot13Table = [('A','N'), ('B','O'), ('C','P'), ('D','Q'), ('E','R'),
              ('F','S'), ('G','T'), ('H','U'), ('I','V'), ('J','W'),
              ('K','X'), ('L','Y'), ('M','Z'), ('N','A'), ('O','B'),
              ('P','C'), ('Q','D'), ('R','E'), ('S','F'), ('T','G'),
              ('U','H'), ('V','I'), ('W','J'), ('X','K'), ('Y','L'),
              ('Z','M')]

tableToFunc :: [(Char, Char)] -> Char -> Char
tableToFunc t c = snd (head (filter (\ ( st, dr ) -> (st == c) ) t ) ) 

substCrypt :: [(Char, Char)] -> String -> String

substCrypt = map . tableToFunc 

{-
  5. (2p)
  Implementați funcția xorCrypt. Pentru un șir de caractere și o cheie dată,
  funcția va întoarce o listă de Word8 reprezentând textul criptat cu XOR pe
  biți între cheie și text.

  Observație: Implementați funcția „point-free” (doar primul parametru al
  funcției va fi explicit).

  Observație: Se presupune că textul și cheia au aceeași lungime.

  Hint: Folosiți-vă de funcțiile de calcul pe biți din modulul Data.Bits.
  http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Data-Bits.html
-}

check5 :: TestPP ()
check5 = do
  assertVal "[5] xorCrypt" 2 $ -- 2p
    (xorCrypt "This is a super secret string!" $ take 30 $ repeat 2) ==
    [86,106,107,113,34,107,113,34,99,34,113,119,114,103,112,34,113,103,97,112,
    103,118,34,113,118,112,107,108,101,35]

xorCrypt :: String -> [Word8] -> [Word8]
xorCrypt s =  zipWith ( \ el1 el2 -> (charToWord el1) `xor` el2 ) s    

{-
  6. (BONUS, 2p)
  Implementați xorCrypt2, o variantă a funcției xorCrypt care poate primi chei
  de lungime arbitrară. Dacă lungimea cheii e mai mică decât cea a textului,
  atunci se va face wrap-around la cheie. Ex: cheia "abc" de lungime 5 va
  deveni "abcab".

  Observație: Este obligatoriu să reutilizați xorCrypt pentru implementarea lui
  xorCrypt2.
-}

check6 :: TestPP ()
check6 = do
  assertVal "[6] xorCrypt2" 2 $ -- 2p
    (xorCrypt2 "This is a super secret string!" $ take 20 $ repeat 2) ==
    [86,106,107,113,34,107,113,34,99,34,113,119,114,103,112,34,113,103,97,112,
    103,118,34,113,118,112,107,108,101,35]

xorCrypt2 :: String -> [Word8] -> [Word8]
xorCrypt2 s k = xorCrypt s (take (length s) (cycle k) )

{-
  7. (BONUS, 3p)
  Expresii regulate 
  Se dă un alfabet sub formă de listă de caractere (simboluri). Generați fluxul care conține
  toate șirurile care se pot forma cu acest alfabet în ordine crescătoare a lungimii șirurilor.

  Ex: Pentru alfabetul ['a', 'b'], limbajul generat cu ajutorul simbolurilor din acest alfabet este
      ["","a","b","aa","ba","ab","bb","aaa","baa","aba","bba","aab","bab","abb","bbb","aaaa","baaa","abaa","bbaa","aaba"]
-}

check7 :: TestPP ()
check7 = do
  assertVal "[7] regular expressions 2" 1 $ -- 1p
    (take 15 $ language ['0', '1']) == ["","0","1","00","10","01","11","000","100","010","110","001","101","011","111"]
  assertVal "[7] regular expressions 3" 2 $ -- 2p
    (take 20 $ language ['a', 'b', 'c']) == ["","a","b","c","aa","ba","ca","ab","bb","cb","ac","bc","cc","aaa","baa","caa","aba","bba","cba","aca"]

language :: [Char] -> [String]
language alf = concat ( iterate (\ allList -> concatMap ( \ x -> helps x alf ) allList ) [ "" ] )


--primeste toate sirurile de lungime maxim x, returneaza toate sirurile de lungiem amxim x + 1
--helps [] el = el

helps lista alfa = map ( \ litera -> (litera : lista) ) alfa


--ajutor lista el = [ el ] : (helps lista el)
{-
Helpers for testing :)
-}
runAllTests = runTestPP $
  sequence_ [check1, check2, check3, check4, check5, check6, check7]
