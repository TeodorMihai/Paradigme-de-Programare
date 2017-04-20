import Data.Array

data StateInfo = StInf 
    { val :: Float
    , cnt :: Int
    } deriving (Show)

type Estimation = Array  Int StateInfo
reinforcements = array (1, 12) [ (1,0), (2,0), (3,0), (4,0), (5,0), (6,0), (7,0), (8,-1), (9,0), (10,0), (11,0), (12,1)]

{--est = array (1, 12) [ (  | i <- reinforcements ]--}
x = fmap (\ x -> StInf x 0 ) reinforcements

t = fmap (val . (x !)) [1..12]
alfa = 0.4


put :: Int -> Float -> Float
put x y 
    | x == 0 = 1.0
    | x == 1 = y
    | x `mod` 2 == 1 = p * y
    | x `mod` 2 == 0 = p
        where t = put (x `div` 2) y
              p = t * t