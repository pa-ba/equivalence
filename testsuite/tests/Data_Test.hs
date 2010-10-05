module Main where

import Test.Framework
import qualified Data.Equivalence.Monad_Test

--------------------------------------------------------------------------------
-- Test Suits
--------------------------------------------------------------------------------

main = defaultMain [tests]

tests = testGroup "Data" [
         Data.Equivalence.Monad_Test.tests
       ]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------