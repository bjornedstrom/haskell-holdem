module Main (main) where

import Test.Framework (defaultMain)
import Suite (tests)

main :: IO ()
main = defaultMain tests
