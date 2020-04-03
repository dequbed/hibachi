module Main (main) where

import Prelude.FilePath

import Data.H2O.Shake.Init
import Data.H2O.Shake.Branch

import Development.Shake

import Templates

-- Paths
baseDir :: FilePath
baseDir = "out"
robotstxt :: FilePath
robotstxt = baseDir </> "robots.txt"
abouthtml :: FilePath
abouthtml = baseDir </> "about.html"

main :: IO ()
main = hibachiBuild "/home/glr/Documents/Blog/posts" $ do
    robotstxt %> \out ->
        writeFileUtf8 out =<< getVersionedFile "static" "robots.txt"
    abouthtml %> \out ->
        writeFileUtf8 out =<< aboutTemplate <$> getVersionedFile "static" "about.md"

    want [robotstxt, abouthtml]
