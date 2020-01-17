module Main where

import Test.H2O

main :: IO ()
main = do
    runTestTT tests
    return ()
