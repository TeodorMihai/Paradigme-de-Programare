module Graph where

import Data.List

{-
    Următoarele exerciții vor avea drept scop implementarea unei mici biblioteci
    pentru grafuri ORIENTATE.
    După cum știți, există mai multe modalități de reprezentare a unui graf.
    Biblioteca noastră va defini mai multe astfel de reprezentări, precum
    și algoritmi care operează pe grafuri.
    Algoritmii vor fi independenți de reprezentarea internă a grafului - ei
    vor funcționa indiferent de ce structură de date am ales noi pentru un anume
    graf.
    Pentru a obține această genericitate vom abstractiza noțiunea de graf într-o
    clasă care va expune operațiile pe care orice structură de date de tip graf
    ar trebui să le aibă.
-}

-- reprezentăm nodurile ca întregi
type Node = Int

-- reprezentăm arcele ca perechi de noduri
type Arc = (Node, Node)

-------------------------------------------------------------------------------

{-
    1.(1.5p) Analizați clasa Graph definită mai jos și scrieți implementările
    implicite pentru funcțiile din această clasă.

    Clasa Graph definește interfața pentru toate structurile de grafuri
    pe care le vom implementa mai jos.
-}
class Graph g where

    -- Construieste un graf plecand de la o lista de noduri si arcele dintre noduri
    build :: [Node] -> [Arc] -> g

    -- Lista tuturor nodurilor din graf
    nodes :: g -> [Node] -- lista nodurilor din graf

    -- Lista arcelor din graf
    arcs :: g -> [Arc] -- lista muchiilor din graf

    -- Lista nodurilor catre care nodul dat ca parametru are un arc
    nodeOut :: g -> Node -> [Node]

    -- Lista nodurilor care au un arc catre nodul dat ca parametru
    nodeIn :: g -> Node -> [Node]

    -- Verifica daca exista un arc intre 2 noduri
    arcExists :: g -> Node -> Node -> Bool

    -- TODO: implementari default
    arcExists g a b = undefined
    arcs g = undefined
    nodeIn g n = undefined
    nodeOut g n = undefined

-------------------------------------------------------------------------------

{-
    2.(2p) Definiți tipul AdjListGraph care reprezintă un graf ca pe o serie de
    perechi (nod, listă vecini). Includeți AdjListGraph în clasa Graph
-}
-- TODO: de rafinat parametrul lui ALGraph
newtype AdjListGraph = ALGraph Node deriving Show

instance Graph AdjListGraph where
  -- TODO

-------------------------------------------------------------------------------

{-
    3. (1p) Definiți tipul ArcGraph care reprezintă un graf ca o listă de noduri
    și o listă de arce între noduri. Includeți ArcGraph în clasa Graph
-}
-- TODO: de rafinat constructorul de date AGraph
data ArcGraph = AGraph deriving Show

instance Graph ArcGraph where
  -- TODO

-------------------------------------------------------------------------------

{-
    4. (0.5p) Definiți funcția convert care face conversia între
    reprezentări diferite de grafuri.
-}
convert :: (Graph g1, Graph g2) => g1 -> g2
convert g1 = undefined -- TODO


-------------------------------------------------------------------------------
{-
    O traversare a unui graf este o listă de perechi (nod, Maybe
    părinteNode). Această structură va conține toate nodurile rezultate în
    urma unei parcurgeri a unui graf, în ordinea apariției în parcurgere și
    împreuna cu părintele nodului din parcurgere (Pentru un nod N, părintele
    său este nodul din care s-a ajuns la N în decursul parcurgerii)
-}
type Traversal = [(Node, Maybe Node)]

-- O cale în graf este reprezentată ca listă de noduri din acea cale.
type Path = [Node]

{-
    Definiție pentru algoritmi de parcurgere a unui graf. Un algoritm de
    parcurgere este o funcție care primește un graf și un nod de start și
    întoarce o structură Traversal. Observați că tipul grafului este o
    variabilă de tip - algoritmii trebuie să funcționeze pentru orice
    structură de tip graf.
-}
type TraverseAlgo g = g -> Node -> Traversal

-------------------------------------------------------------------------------

{-
    5. (2.5p) Implementați algoritmul de parcurgere în adâncime. (Depth-First Search)
-}
dfs :: Graph g => TraverseAlgo g
dfs g start = undefined -- TODO


{-
    6. (2.5p) Implementați algoritmul de parcurgere în lățime. (Breadth-First Search)
-}
bfs :: Graph g => TraverseAlgo g
bfs g start = undefined -- TODO


-------------------------------------------------------------------------------

{-
    7. (BONUS 1p) Definiți funcția findPath care primește ca parametri:
    * un algoritm de parcurgere
    * un graf
    * un nod de pornire
    * un nod destinație
    și care întoarce o cale dintre cele două noduri, dacă o astfel de cale
    există.
-}
findPath :: Graph g => TraverseAlgo g -> g -> Node -> Node -> Maybe Path
findPath traverse g n1 n2 = undefined -- TODO

-------------------------------------------------------------------------------

{-
    8. (BONUS 1p). Creați tipul GenericAdjListGraph care este este similar cu
    AdjListGraph, dar în care nodurile nu sunt neapărat de tip Int - pot
    fi de orice tip.
-}
-- TODO: de rafinat parametrul lui GALGraph
newtype GenericAdjListGraph a = GALGraph a deriving Show

{-
    9. (BONUS 1p). Adăugați tipul GenericAdjListGraph la clasa Functor
-}
instance Functor GenericAdjListGraph where
  -- TODO: de definit fmap
  fmap = undefined

{-
    10. (BONUS 2p). Definiți clasa Listable pentru containere care conțin elemente
    și care va avea o singură funcție: toList - care întoarce lista elementelor
    conținute în container. Adăugați GenericAdjListGraph la această clasă.
-}
class Listable c where
  -- TODO: de declarat tipul lui toList

-- instance Listable GenericAdjListGraph where
--  -- TODO: de definit toList pentru GenericAdjListGraph
