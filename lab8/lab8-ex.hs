{-
  PP, laboratorul 8: tipuri de date utilizator
-}

import Data.List
import TestPP

{-
  1. (2p) Numere complexe
  Se dă tipul de date Complex, reprezentând numerele complexe, i.e.
  primul câmp îl reprezintă partea reală, iar cel de-al doilea partea
  imaginară.

  Implementați următoarele operații cu numere complexe:
  - compararea la egalitate (eqC) între două numere
  - adunarea (addC) și scăderea (subC) a două numere
  - înmulțirea (mulC) și împărțirea (divC) a două numere
  - conjugata (conjC) unui număr
  - modulul (absC) unui număr

  Pentru mai multe detalii, consultați:
  https://en.wikipedia.org/wiki/Complex_number#Elementary_operations
-}
check1 :: TestPP ()
check1 = do
  let c1 = C 3 2
      c2 = C 4 5
  let eqR a b = abs (a - b) < 0.0001
  assertVal "[1] eqC" 0.5 $ -- 0.5p
    eqC c1 c1
  assertVal "[1] addC, subC" 0.25 $ -- 0.25p
    ((c1 `addC` c2) `subC` (C 1 1)) `eqC` (C 6 6)
  assertVal "[1] mulC" 0.25 $ -- 0.25p
    (c1 `mulC` c2) `eqC` (C 2 23)
  assertVal "[1] divC" 0.25 $ -- 0.25p
    (c2 `divC` c1) `eqC` (C 1.6923 0.5384)
  assertVal "[1] conjC" 0.25 $ -- 0.25p
    (conjC c2) `eqC` (C 4 (-5))
  assertVal "[1] absC" 0.5 $ -- 0.5p
    (absC c2) `eqR` 6.4031

data Complex = C
  { realC :: Double
  , imgC  :: Double
  } deriving Show

-- Hint: dat fiind că numerele complexe sunt definite pe baza a două
-- numere în virgulă mobilă, puteți implementa egalitatea aproximativă
-- dintre două numere, folosind absC și subC.
eqC :: Complex -> Complex -> Bool
eqC c1 c2 =  (absC dif)  <= eps
  where dif = (subC c1 c2)
        eps = 0.0001


addC :: Complex -> Complex -> Complex
addC c1 c2 = (C s1 s2)
  where s1 = (realC c1) + (realC c2)
        s2 = (imgC c1) + (imgC c2)

subC :: Complex -> Complex -> Complex
subC c1 c2 = (C s1 s2)
  where s1 = (realC c1) - (realC c2)
        s2 = (imgC c1) - (imgC c2)

mulC :: Complex -> Complex -> Complex
mulC c1 c2 = C ( ((realC c1) * (realC c2) ) - ( (imgC c1) * (imgC c2)) ) ( (realC c1) * (imgC c2) + (realC c2) * (imgC c1) )

divC :: Complex -> Complex -> Complex
divC c1 c2 = C ( (realC p) / d) ( (imgC p) / d)
  where d = realC (mulC c2 (conjC c2))
        p = mulC c1 (conjC c2)

conjC :: Complex -> Complex
conjC c1 = C (realC c1)  ( (-1) * (imgC c1))

absC :: Complex -> Double
absC c1 = sqrt ( (realC c1) * (realC c1)  + ( (imgC c1) * (imgC c1) ) )

{-
 2. (4p) Liste imbricate
  Definiți un tip de date SList a care să aibă funcționalități
  asemănătoare listelor din limbajele Lisp (e.g. Scheme, Racket, Clojure),
  permițând componente la diferite niveluri de imbricare.
  Ex: Lista din Racket '(1 (3 4) (2)) să poată fi definită în Haskell
  folosind SList.
  Adițional, definiți:
  - emptySList, lista vidă
  - consElem, adaugă un element în capul unei liste
    Ex: consElem 1 '((3 4) (2)) == '(1 (3 4) (2))
  - consList, adaugă o listă (imbricată) în capul unei liste
    Ex: consList '(2 3) '(1 2) == '((2 3) 1 2)
  - headSList, ia primul element dintr-un SList
  - tailSList, ia restul SList-ului
  - deepEqual, o funcție ce verifică egalitatea a două SList
  - flatten, întoarce lista cu elementele din SList (pe același nivel)
  Notare:
  (2p) constructorii (emptySList, consElem și consList) și deepEqual
  (1p) headSList și tailSList
  (1p) flatten
-}
check2 :: TestPP ()
check2 = do
  let l1 = consElem 1 $ emptySList
      l2 = consElem 2 $ consList (consElem 1 $ consElem 1 emptySList) $
           consElem 3 emptySList
      l3 = consList (consElem 1 $ consElem 1 emptySList) $ consElem 3 $
           emptySList
  assertVal "[2] simple lists" 1 $ -- 1p
    deepEqual l1 l1 && not (deepEqual l1 l2)
  assertVal "[2] less simple lists" 1 $ -- 1p
    deepEqual (consElem 2 $ l3) l2
  assertVal "[2] head, tail" 1 $ -- 1p
    deepEqual (headSList $ tailSList l2)
              (consElem 1 $ consElem 1 emptySList)
  assertVal "[2] flatten" 1 $ -- 1p
    flatten l2 == [2,1,1,3]


data SList a =  Void | Cons a (SList a) | Imbr (SList a) (SList a)  deriving Show

emptySList :: SList a
emptySList = Void 

consElem :: a -> SList a -> SList a
consElem a l = Cons a l

consList :: SList a -> SList a -> SList a
consList l1 l2 = Imbr l1 l2

headSList :: SList a -> SList a
headSList Void = Void 
headSList (Cons a l) = Cons a Void
headSList (Imbr l1 l2) = l1

tailSList :: SList a -> SList a
tailSList Void = Void
tailSList (Imbr l1 l2) = l2
tailSList (Cons a l1) = l1

deepEqual :: Eq a => SList a -> SList a -> Bool
deepEqual Void Void = True
deepEqual (Cons a1 l1) (Cons a2 l2) = (a1 == a2) && (deepEqual l1 l2)
deepEqual (Imbr l1 l2) (Imbr l3 l4) = (deepEqual l1 l3) && (deepEqual l2 l4)
deppEqual  _ _ = False

flatten :: SList a -> [a]
flatten Void = []
flatten (Cons a l1) = (a : flatten l1)
flatten (Imbr l1 l2) = (flatten l1) ++ (flatten l2)


{-
  3. (4p) Arbori binari de căutare
  Definiți un tip de date BST a pentru a implementa un arbore binar de
  căutare.  De asemenea, definiți funcții pentru a crea un arbore binar de
  căutare pornind de la o listă de elemente, căutarea unui element într-un
  arbore binar de căutare dat și o funcție care întoarce lista elementelor
  din parcurgerea în inordine a arborelui.

  Hint: Este de preferat ca arborele binar de căutare să fie echilibrat,
  lucru ușor de obținut la creare dacă lista de elemente este sortată.
-}
check3 :: TestPP ()
check3 = do
  let bst = makeBST [2,3,1,4]
  assertVal "[3] findElem" 2 $ -- 2p
    findElem bst 1 == Just 1
  assertVal "[3] inorder" 2 $ -- 2p
    inorder bst == [1,2,3,4]

data BST a = VoidBST | ConsBST a (BST a) (BST a) | Uneste (BST a) (BST a) deriving Show

insertBST :: Ord a => a -> BST a -> BST a
insertBST a VoidBST = ConsBST a VoidBST VoidBST
insertBST el (Cons a b1 b2)
  | el < a = ConsBST a (insertBST el b1) b2
  | el >= a = ConsBST a b1 (insertBST el b2)

makeBST :: Ord a => [a] -> BST a
makeBST [] = VoidBST
makeBST a = map insertBST a

findElem :: (Ord a, Eq a) => BST a -> a -> Maybe a
findElem a VoidBST = ConsBST a VoidBST VoidBST
findElem el (Cons a b1 b2)
  | el < a = ConsBST a (insertBST el b1) b2
  | el >= a = ConsBST a b1 (insertBST el b2)

inorder :: BST a -> [a]
inorder = undefined

{-
  4. (BONUS, 2p) Arbori binari cu valori în frunze
  Având dat tipul BinaryTree a din cadrul exercițiilor rezolvate, definiți
  funcția subtree, care verifică dacă arborele t1 este un subarbore al
  arborelui t2.
  Ex: subtree (makeBinTree [1,2]) (makeBinTree [1,2,3,4]) == True
-}
check4 :: TestPP ()
check4 = do
  let t = makeBinTree [1,2,3,4]
  assertVal "[4] subtree leaf" 0.5 $ -- 0.5p
    subtree (Leaf 1) t
  assertVal "[4] subtree [1,2]" 0.5 $ -- 0.5p
    subtree (makeBinTree [1,2]) t
  assertVal "[4] subtree [3,4]" 0.5 $ -- 0.5p
    subtree (makeBinTree [3,4]) t
  assertVal "[4] not $ subtree [2,3]" 0.5 $ -- 0.5p
    not $ subtree (makeBinTree [2,3]) t

data BinaryTree a = Node (BinaryTree a) (BinaryTree a) | Leaf a deriving Show

makeBinTree :: [a] -> BinaryTree a
makeBinTree lst = head $ mergeUpwards leafList
    where
      leafList = map (\x -> Leaf x) lst
      mergeUpwards [] = []
      mergeUpwards [x] = [x]
      mergeUpwards (x:y:xs) = mergeUpwards ( (Node x y) : mergeUpwards xs)

subtree :: Eq a => BinaryTree a -> BinaryTree a -> Bool
subtree = undefined

{-
  5. (BONUS, 3p) Difference lists
  Se cere definirea tipului de date "difference list".

  Un difference list este o listă "parțial construită", i.e. ale cărei
  elemente din coadă nu sunt (neapărat) în întregime cunoscute. De
  exemplu, ne putem imagina existența unei liste:

  1 : (2 : (3 : xs)) = [1,2,3] ++ xs

  unde xs nu are (la momentul construirii) o valoare cunoscută.

  În limbajele funcționale putem modela difference lists folosindu-ne de
  închideri: putem privi o astfel de structură ca pe o funcție care
  așteaptă un parametru (o listă) și întoarce o listă. Exemplul anterior
  poate fi astfel exprimat în funcție drept următoarea listă:

  (\ xs -> [1,2,3] ++ xs)

  Observație: Care este tipul lambda-ului de mai sus?

  Avantajul acestei abordări este că permite efectuarea oricărei
  operație de adăugare în listă (e.g. concatenarea cu o altă listă) în
  O(1), cu dezavantajul că eliminarea este în general mai puțin eficientă,
  deoarece presupune evaluarea elementelor structurii.

  Se cere, mai concret:
  - Definirea ADT-ului difference list (DList), „împăturit peste” o
    funcție de tipul [a] -> [a] (e.g. folosind construcția newtype)
  - Conversia [a] -> DL a (dlFromList) și invers (dlToList)
  - Lista vidă (emptyDL), adăugarea în capul unei liste (consDL) și în
    coada ei (snocDL)
  - Concatenarea a două liste (appendDL)
  - Operații de eliminare: primul element (headDL) și coada (tailDL)
    unei liste

  Operațiile de lucru cu difference lists (cu excepția celor de
  eliminare) vor fi implementate cât mai eficient posibil, i.e. fără a
  folosi dlFromList și dlToList.

  Pentru mai multe detalii, consultați link-ul:
  https://wiki.haskell.org/Difference_list
-}
check5 :: TestPP ()
check5 = do
  assertVal "[5] toList, fromList" 0.5 $ -- 0.5p
    dlToList (dlFromList "Ana are mere") == "Ana are mere"
  assertVal "[5] cons, empty" 0.5 $ -- 0.5p
    dlToList (consDL 1 $ consDL 2 $ consDL 3 emptyDL) == [1,2,3]
  assertVal "[5] snoc, empty" 0.5 $ -- 0.5p
    dlToList (snocDL 1 $ snocDL 2 $ snocDL 3 emptyDL) == [3,2,1]
  assertVal "[5] append" 0.5 $ -- 0.5p
    dlToList (dlFromList [1,2,3] `appendDL` dlFromList [4,5,6]) ==
             [1,2,3,4,5,6]
  assertVal "[5] head, tail" 1 $ -- 1p
    -- (uglier, but at the expense of ease of understanding)
    case tailDL (dlFromList [1,2,3,4,5]) of
    Just dl -> case tailDL dl of
               Just dl -> headDL dl == Just 3

newtype DList a = DL a -- de rafinat tipul argumentului lui DL

dlFromList :: [a] -> DList a
dlFromList = undefined

dlToList :: DList a -> [a]
dlToList = undefined

emptyDL :: DList a
emptyDL = undefined

consDL :: a -> DList a -> DList a
consDL = undefined

snocDL :: a -> DList a -> DList a
snocDL = undefined

appendDL :: DList a -> DList a -> DList a
appendDL = undefined

headDL :: DList a -> Maybe a
headDL = undefined

tailDL :: DList a -> Maybe (DList a)
tailDL = undefined

{-
Helpers for testing :)
-}
runAllTests = runTestPP $
  sequence_ [check1, check2, check3, check4, check5]
check = runAllTests
