module Test.H2O 
    ( module Test.HUnit
    , tests
    ) where

import Test.HUnit
import Data.Time.Clock
import Data.Binary
import Data.H2O
import qualified Data.ByteString.Lazy as BS

testCodeTime = TestCase (do
    t <- getCurrentTime
    let u = decode $ encode t
    assertEqual "Time encoded and decoded is equal" t u
    )

tests = TestList [ TestLabel "Time en-/decoding" testCodeTime ]
