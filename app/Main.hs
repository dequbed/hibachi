module Main (main) where

import qualified Prelude.Text as T
import qualified Prelude.List as L
import Prelude.FilePath

import Data.H2O
import Data.H2O.Shake

import Development.Shake

import Templates

main :: IO ()
main = hibachiBuild "/home/glr/Documents/Blog/posts" $ do
    return ()
