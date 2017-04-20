module TestPP 
  ( TestPP, runTestPP
  , assertValIO, assertVal, assertProp
  ) where

import qualified Control.Exception as E (catch, evaluate, SomeException (..))
import Test

-- Keep the grade in testData
type TestPP a = Test Float a

getTestData :: TestPP a -> TestData Float
getTestData t = execTest t emptyTD

printTestCase :: TestCase Float -> IO (TestCase Float)
printTestCase tc = do
  putStr $ testName tc
  putStr $ " [" ++ show (testData tc) ++ "]: "
  E.catch (do
             tp <- testPassed tc
             putStrLn $ getPassed tp
             return $ if tp then tc else tc { testResult = 0 })
          (\ (E.SomeException e) -> do
             print e
             return tc { testResult = 0 })
  where
  getPassed p = if p then "PASSED"
                     else "FAILED"

printFinalGrade :: TestData Float -> IO ()
printFinalGrade td = do
  putStr $ "Final grade: "
  print $ sum $ map testResult td

runTestPP :: TestPP a -> IO ()
runTestPP t = do
  let td = getTestData t
  td' <- mapM printTestCase td
  printFinalGrade td'
