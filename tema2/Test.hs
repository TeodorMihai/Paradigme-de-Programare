{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Test
  ( Test, TestCase (..), TestData, emptyTD
  , assertValIO, assertVal, assertProp
  , runTest, evalTest, execTest
  ) where

import Control.Applicative
import qualified Control.Monad.Trans.State.Lazy as S
import Control.Monad.IO.Class

class (Monad m) => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

instance (Monad m) => MonadState s (S.StateT s m) where
  get = S.get
  put = S.put

modify :: MonadState s m => (s -> s) -> m ()
modify f = do
  s <- get
  put (f s)

runState = S.runState

newtype Test d a = MkTest { getTestState :: S.State (TestData d) a }
  -- The Holy Trinity + MonadState
  deriving (Functor, Applicative, Monad, MonadState (TestData d))

data TestCase a = TestCase
  { testName    :: String
  , testPassed  :: IO Bool
  , testData    :: a
  , testResult  :: a
  }

type TestData a = [TestCase a]

emptyTD :: TestData a
emptyTD = []

-- Assertion primitives
assertValIO :: String -> d -> IO Bool -> Test d ()
assertValIO s d b = modify $ \ tcs -> tcs ++ [TestCase s b d d]

assertVal :: String -> d -> Bool -> Test d ()
assertVal s d b = assertValIO s d (return b)

assertProp :: String -> d -> (a -> Bool) -> a -> Test d ()
assertProp s d p x = assertVal s d $ p x

-- State-style de-lifting functions
runTest :: Test d a -> TestData d -> (a, TestData d)
runTest t = runState $ getTestState t

evalTest :: Test d a -> TestData d -> a
evalTest t = fst . runTest t

execTest :: Test d a -> TestData d -> (TestData d)
execTest t = snd . runTest t
