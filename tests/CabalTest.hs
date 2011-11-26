module Main (main) where

import Data.Monoid (mempty)
import Test.Framework

import Suite (tests)

main :: IO ()
main = defaultMainWithOpts tests options
    where options = (mempty :: RunnerOptions) {ropt_plain_output = Just True}
