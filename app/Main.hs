module Main (main) where

import Prelude.FilePath
import Prelude.Directory

import Data.H2O.Shake.Init
import Data.H2O.Shake.Branch

import Development.Shake

import Templates
import Style

-- Paths
baseDir :: FilePath
baseDir = "out"
robotstxt :: FilePath
robotstxt = baseDir </> "robots.txt"
abouthtml :: FilePath
abouthtml = baseDir </> "about.html"
stylecss :: FilePath
stylecss = baseDir </> "css" </> "default.css"

main :: IO ()
main = hibachiBuild "/home/glr/Documents/Blog/posts" $ do
    robotstxt %> \out ->
        writeFileD out =<< getVersionedFile "static" "robots.txt"
    abouthtml %> \out ->
        writeFileD out =<< aboutTemplate <$> getVersionedFile "static" "about.md"
    stylecss %> \out ->
        writeFileD out styleText

    want [robotstxt, abouthtml, stylecss]

-- | Write a file, creating the directory containing it if necessary
writeFileD :: FilePath -> Text -> Action ()
writeFileD file content = do
    createDirectoryIfMissing True (takeDirectory file)
    writeFileUtf8 file content
