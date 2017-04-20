module RLTest where

import RL
import TestPP
import Data.Array
import Data.List
import System.Random
import Control.Arrow ((&&&))
import Control.Monad

-- Partea 1

testNeighborsOf :: TestPP ()
testNeighborsOf = do
    assertVal "neighborsOf.7" 2.5 $ sort (neighborsOf 7) == [3, 8, 11]
    assertVal "neighborsOf.6" 2.5 $ neighborsOf 6 == []

testRandomPath :: TestPP ()
testRandomPath = do
    -- Începe cu starea inițială
    assertValIO "randomPath.start" 3 $ do
        path <- getStdRandom $ randomPath
        return $ head path == startState
    -- Se poate genera oricât se dorește din cale
    assertValIO "randomPath.infinite" 3 $ do
        path <- getStdRandom $ randomPath
        len <- randomRIO (10^2, 10^4)
        return $ length (take len path) == len
    -- Calea este construită corect
    assertValIO "randomPath.neighbors" 6 $ do
        path <- getStdRandom $ randomPath
        let finitePath = take 10 path
        return $ all (\(from, to) -> to `elem` neighborsOf from) $
            zip finitePath $ tail finitePath
    -- A doua cale este diferită de prima
    assertValIO "randomPath.different" 3 $ do
        path1 <- getStdRandom $ randomPath
        path2 <- getStdRandom $ randomPath
        return $ take 10 path1 /= take 10 path2

testTerminatePath :: TestPP ()
testTerminatePath = do
    assertVal "terminatePath.fixed" 2.5 $ terminatePath [1 .. 12] == [1 .. 8]
    -- Ultima stare trebuie să fie o terminală, iar toate celelalte, nu
    assertValIO "terminatePath.random" 2.5 $ do
        path <- getStdRandom $ randomPath
        let terminatedPath = terminatePath path
        return $ (last terminatedPath `elem` terminalStates) &&
                 (all (`notElem` terminalStates) $ init terminatedPath)

testRandomPaths :: TestPP ()
testRandomPaths = do
    -- Se poate genera oricât se dorește din cale
    assertValIO "randomPaths.infinite" 2.5 $ do
        paths <- getStdRandom $ randomPaths &&& id
        len <- randomRIO (10^2, 10^4)
        return $ length (take len paths) == len
    -- Căile generate sunt diferite
    assertValIO "randomPaths.different" 2.5 $ do
        paths <- getStdRandom $ randomPaths &&& id
        let truncatedPaths = take 10 $ map (take 10) paths
        return $ nub truncatedPaths == truncatedPaths

-- Partea 2

testReinforcements :: TestPP ()
testReinforcements = do
    let pairs = assocs reinforcements
    assertVal "reinforcements" 3 $
        length pairs == nStates &&
        reinforcements ! 8 == -1 &&
        reinforcements ! 12 == 1 &&
        (all ((== 0) . snd) $ filter ((`notElem` terminalStates) . fst) pairs)

testInitialEstimation :: TestPP ()
testInitialEstimation = do
    let pairs = assocs reinforcements
    assertVal "initialEstimation" 2 $
        pairs == zip [1 .. 12] (values initialEstimation)

testShowEstimation :: TestPP ()
testShowEstimation = do
    assertValIO "showEstimation" 5 $ do
        let string = showEstimation initialEstimation
        putStrLn $ '\n' : string
        return $ string == "+0.00 +0.00 +0.00 +1.00\n\
                           \+0.00 +0.00 +0.00 -1.00\n\
                           \+0.00 +0.00 +0.00 +0.00"

testUpdateEstimation :: TestPP ()
testUpdateEstimation = do
    let path = [1, 2, 3, 4, 8]
        estimation1 = updateEstimation initialEstimation path
        estimation2 = updateEstimation estimation1 path
    assertValIO "updateEstimation.1" 5 $ do
        printEstimation estimation1
        return $ values estimation1 ~=
            [0.0,0.0,0.0,-0.1,0.0,0.0,0.0,-1.0,0.0,0.0,0.0,1.0]
    assertValIO "updateEstimation.2" 5 $ do
        printEstimation estimation2
        return $ values estimation2 ~=
            [0.0,0.0,-0.01,-0.19,0.0,0.0,0.0,-1.0,0.0,0.0,0.0,1.0]

testEstimations :: TestPP ()
testEstimations = do
    assertValIO "estimations" 5 $ do
        paths <- getStdRandom (randomPaths &&& id)
        let errors = take 2000 $ map (rmse . values) $ estimations $
                map terminatePath paths
            Just index = findIndex (\e -> e / head errors < 0.1) errors
        putStrLn $ "\nEroarea (RMSE) a scazut la sub 10% din cea initiala"
            ++ " dupa " ++ show index ++ " pasi"
        return True

testEstimate :: TestPP ()
testEstimate = do
    assertValIO "estimate" 5 $ do
        estimation <- getStdRandom (estimate 1000 &&& id)
        putStr "\nEstimarea 1000:"
        printEstimation estimation
        return True

testBestNeighborOf :: TestPP ()
testBestNeighborOf = do
    assertValIO "bestNeighborOf.1" 2.5 $ do
        estimation <- getStdRandom (estimate 1000 &&& id)
        return $ bestNeighborOf 1 estimation == 5
    assertValIO "bestNeighborOf.7" 2.5 $ do
        estimation <- getStdRandom (estimate 1000 &&& id)
        return $ bestNeighborOf 7 estimation == 11

testBestPath :: TestPP ()
testBestPath = do
    assertValIO "bestPath" 5 $ do
        estimation <- getStdRandom (estimate 1000 &&& id)
        return $ terminatePath (bestPath estimation) == [1, 5, 9, 10, 11, 12]

testScaledLearningRates :: TestPP ()
testScaledLearningRates = do
    -- Se poate genera oricât se dorește din flux
    assertValIO "scaledLearningRates.infinite" 2.5 $ do
        len <- randomRIO (10^2, 10^4)
        return $ length (take len scaledLearningRates) == len
    -- Câteva valori
    assertVal "scaledLearningRates.values" 2.5 $ take 4 scaledLearningRates ~=
        [ 1
        , learningRate
        , learningRate * scaleFactor
        , learningRate * scaleFactor^2
        ]

printEstimation :: Estimation -> IO ()
printEstimation = putStrLn . ('\n' :) . showEstimation

-- Aproximate equality
(~=) :: (Fractional a, Ord a) => [a] -> [a] -> Bool
xs ~= ys
    | length xs == length ys = all (< 0.001) $ map abs $ zipWith (-) xs ys
    | otherwise              = False

-- Root mean squared error between an estimation and the exact values.
rmse :: [Float] -> Float
rmse vals = sqrt $ mean $ map (^2) $ zipWith (-) vals exactValues
  where
    exactValues = [-0.29,-0.42,-0.54,-0.77,-0.16,0,-0.44,-1,-0.04,0.09,0.22,1]
    mean = liftM2 (/) sum genericLength
        
main :: IO ()
main = runTestPP $ sequence_ [ testNeighborsOf
                             , testRandomPath
                             , testTerminatePath
                             , testRandomPaths
                             , testReinforcements
                             , testInitialEstimation
                             , testShowEstimation
                             , testUpdateEstimation
                             , testEstimations
                             , testEstimate
                             , testBestNeighborOf
                             , testBestPath
                             , testScaledLearningRates
                             ]