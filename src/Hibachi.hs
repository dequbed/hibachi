module Hibachi where

import Hibachi.Post
import Hibachi.Read
import Data.Time

import Data.Text.IO as TIO


libmain = do
    f <- TIO.readFile "test.md"
    t <- Data.Time.getCurrentTime
    p <- readPost f [] t t
    --print d
    print $ p
