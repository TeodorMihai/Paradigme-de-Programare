module RL where

import Data.Array
import System.Random
import Data.List
import Control.Monad
import Data.Function (fix)
import Text.Printf

{-
    O stare este reprezentată ca un număr întreg.
    O cale este o listă de stări, eventual infinită.

    O estimare reprezintă o mulțime de asocieri (stare, informație aferentă).
    Cu toate că mediul este bidimensional, utilizăm o reprezentare liniară
    a acestuia, bazată pe un `Array`, cu stările indexate ca în figura din 
    enunț.
-}
type State      = Int
type Path       = [State]
type Estimation = Array State StateInfo
{-
    Lățimea și înălțimea mediului, precum și numărul de stări.
-}
width, height, nStates :: Int
width   = 4
height  = 3
nStates = width * height

{-
    Perechile de stări vecine.
-}
neighbors :: [(State, State)]
neighbors = [ (1, 2), (1, 5)
            , (2, 1), (2, 3)
            , (3, 2), (3, 4)
            , (3, 7)
            , (4, 3), (4, 8)
            , (5, 1), (5, 9)
            , (7, 3), (7, 8), (7, 11)
            , (8, 4), (8, 7), (8, 12)
            , (9, 5), (9, 10)
            , (10, 9), (10, 11)
            , (11, 7), (11, 10), (11, 12)
            , (12, 8), (12, 11)
            ]

{-
    Starea de pornire.
-}
startState :: State
startState = 1

{-
     Stările terminale.
-}
terminalStates :: [State]
terminalStates = [8, 12]

{-
    Rata de învățare alfa și factorul de scalare a acesteia.
-}
learningRate, scaleFactor :: Float
learningRate = 0.1
scaleFactor  = 0.999

-------------------------------------------------------------------------------
-- Completați sub această linie.


--  === 1. Generarea căilor ===

{-
    *** TODO ***

    Întoarce toate stările vecine ale unei stări.
-}
neighborsOf :: State -> [State]
neighborsOf x = map snd (filter ( \ (x1, y1) -> ( x == x1) ) neighbors )
{-
    *** TODO ***

    Construiește o cale aleatoare infinită, pe baza unui generator.

    Hint: `Data.List.iterate`, `System.Random.split`.
-}


randomPath :: RandomGen g => g -> (Path, g)
randomPath g = (map snd path , g1)                                                                                                                                                                                          
                    where path = iterate getNextIterate (g2, startState)
                          (g1 , g2) = split g

getNextIterate :: RandomGen g => (g , State) -> (g, State)
getNextIterate (g, state) = (nextGen, selectList !! ( nrGenerated `mod` ( length selectList) ) ) 
                                          where selectList = neighborsOf state
                                                (nrGenerated, nextGen) = next g                                              
{-
    *** TODO ***

    Trunchiază o cale, eventual infinită, la prima stare terminală.
-}


terminatePath :: Path -> Path
terminatePath path =  (takeWhile (\ x -> not ( elem x terminalStates ) ) path ) ++
                        case  find (\ x -> elem x terminalStates) path of 
                            Just n -> [n]
                            Nothing -> []
                    
{-
    *** TODO ***

    Construiește o infinitate de căi infinite.
-}

makeGenAndPath :: RandomGen g => (Path, g) -> (Path, g)
makeGenAndPath (path, g) = randomPath g 

randomPaths :: RandomGen g => g -> [Path]
randomPaths g = map fst ( iterate makeGenAndPath (randomPath g) )


--  === 2. Estimarea utilităților fără diminuarea ratei de învățare ===

{-
    *** TODO ***

    Array cu conscințele specifice fiecărei stări.
-}
reinforcements :: Array State Float
reinforcements = array (1, 12) [ (1,0), (2,0), (3,0), (4,0), (5,0), (6,0), (7,0), (8,-1), (9,0), (10,0), (11,0), (12,1)]

{-
    *** TODO ***

    Valorile inițiale ale stărilor, înaintea rulării algoritmului.
    Se construiesc pe baza array-ului de consecințe.
-}
initialEstimation :: Estimation
initialEstimation = fmap (\ x -> StInf x 0 ) reinforcements
{-
    *** TODO ***

    Lista de utilități provenite dintr-o estimare.
-}
values :: Estimation -> [Float]
values e = fmap (val . (e !)) [1..12]

{-
    *** TODO ***

    Reprezentarea sub formă de șir de caractere a unei estimări.
    Se va întrebuința forma bidimensională, ca în imaginile din enunț.
    De asemenea, utilitățile vor fi rotunjite la 2 zecimale, și vor
    avea semnul inclus.

    Hint: `Text.Printf`.

    Exemplu de rezultat:

    -0.07 +0.06 +0.20 +1.00
    -0.20 +0.00 -0.43 -1.00
    -0.32 -0.45 -0.56 -0.78

    Pentru a vizualiza corect caracterele de linie nouă, aplicați
    în interpretor funcția `putStrLn` asupra șirului obținut.
-}

printSimple x = str  where str = printf "%+.2f " x
printLast x = str where str = printf "%+.2f\n" x
printLastLast x = str where str = printf "%+.2f" x

showEstimation :: Estimation -> String
showEstimation e = showEstimationHelp height 1  (values e)

showEstimationHelp x y l
    | x == 1 && y == width = printLastLast (l !! (width - 1))
    | x /= 1 && y == width = printLast (l !! (x * width - 1 ) )  ++ (showEstimationHelp (x - 1) 1 l)
    | y /= width = printSimple (l !! ( (x - 1) * width  + y - 1) )  ++ (showEstimationHelp x (y + 1) l)


{-
    *** TODO ***

    Actualizează o estimare în urmare parcurgerii unei căi.

    Hint: `Data.Array.accum`.
-}


makeEstimation :: Path -> Estimation -> Estimation
makeEstimation path e 
    | (length path) > 1 =  makeEstimation (tail path) (e // [ (x , StInf value (n + 1) ) ] ) 
    | otherwise = e
    where x = head path
          y = head (tail path)
          vs = val (e ! x)
          vsne = val (e ! y)
          n = cnt (e ! x) 
          r = reinforcements ! x
          learnR = last ( take (n + 2) scaledLearningRates) 
          value = vs +  learnR *  (r + vsne - vs)


updateEstimation :: Estimation -> Path -> Estimation
updateEstimation e path = makeEstimation path e

{-
    *** TODO ***

    Obține un flux infinit de estimări rafinate succesiv, pe baza unui flux
    infinit de căi finite, încheiate în stări terminale.

    Hint: `Data.List.mapAccumL`.
-}
estimations :: [Path] -> [Estimation]
estimations paths = snd (mapAccumL 
                (\ acc x -> let newAcc = (updateEstimation acc x) in (newAcc, newAcc) ) 
                initialEstimation paths)
--}
{-
    *** TODO ***

    Determină estimarea de rang dat ca parametru, pe baza unui generator.
-}
estimate :: RandomGen g => Int -> g -> Estimation
estimate rang g = last (take rang $ estimations $ map terminatePath $ randomPaths g)

{-
    *** TODO ***

    Pentru o stare, determină vecinul cu cea mai mare valoare estimată.

    Hint: `Data.Function.on`.
-}
bestNeighborOf :: State -> Estimation -> State
bestNeighborOf s estimation = foldr
                             (\ acc x ->  if (val (estimation ! x)) < (val (estimation ! acc)) then acc else x)
                             s
                             ( neighborsOf s)

inversBestNeighborsOf :: Estimation -> State -> State
inversBestNeighborsOf e s = bestNeighborOf s e
{-
    *** TODO ***

    Contruiește o cale începută în starea inițială, pe principiul alegerii 
    vecinului cu utilitata maximă.
-}
bestPath :: Estimation -> Path
bestPath estimation = iterate (inversBestNeighborsOf estimation) startState


--  === 3. Estimarea utilităților cu diminuarea ratei de învățare ===

{-
    *** TODO ***

    Fluxul infinit al ratelor de învățare scalate:

    [ 1
    , learningRate
    , learningRate * scaleFactor
    , learningRate * scaleFactor^2
    , ...
    ]
-}

po = 0 : [ g n | n <- [1..] ]
    where g n = if n == 0 then 1 else 
                    if n == 1 then scaleFactor else 
                        if n `mod` 2 == 0 then t * t else t * t * scaleFactor
                            where t = po !! (n `div` 2)


scaledLearningRates :: [Float]
scaledLearningRates = 1.0 : (map (learningRate * ) ( map (po !! ) [1..] ))

{-
    *** TODO ***

    Tip de date pentru reținerea atât a valorii estimate a unei stări,
    cât și a numărului de vizitări ale acesteia.
-}
data StateInfo = StInf 
    { val :: Float
    , cnt :: Int
    } deriving (Show)
