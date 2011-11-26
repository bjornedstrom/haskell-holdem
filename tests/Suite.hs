module Suite (
    tests
) where

import Test.Framework (Test)

import qualified TestCard

tests :: [Test]
tests = [] ++ TestCard.tests
