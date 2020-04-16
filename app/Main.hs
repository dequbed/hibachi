module Main (main) where

import Prelude.FilePath
import Prelude.Directory
import qualified Prelude.List as L
import Data.ByteString.Char8 as BL

import Lens.Micro

import Crypto.Hash

import Data.H2O.Shake
import Data.H2O.Shake.Init
import Data.H2O.Shake.Post
import Data.H2O.Shake.Branch
import Data.H2O.Shake.Index
import Data.H2O.Post
import Data.H2O.Types

import Development.Shake

import Templates
import Style

import System.IO (print)

-- Paths
baseDir :: FilePath
baseDir = "out"
robotstxt :: FilePath
robotstxt = baseDir </> "robots.txt"
abouthtml :: FilePath
abouthtml = baseDir </> "about.html"
stylecss :: FilePath
stylecss = baseDir </> "css" </> "default.css"
indexhtml :: FilePath
indexhtml = baseDir </> "index.html"

basePostDir :: FilePath
basePostDir = baseDir </> "p"

main :: IO ()
main = hibachiBuild "/home/glr/Documents/Blog/posts" $ do
    robotstxt %> \out ->
        writeFileD out =<< getVersionedFile "static" "robots.txt"
    abouthtml %> \out ->
        writeFileD out =<< aboutTemplate <$> getVersionedFile "static" "about.md"
    stylecss %> \out ->
        writeFileD out styleText

    want [robotstxt, abouthtml, stylecss]

    writePost (\p -> do
        let filename = genFileN $ p^.title
            path = basePostDir </> filename <.> "html"

        writeFileD path $ renderPostText p

        return $ pathToLink path)

    genBranchIndex "posts"

    writeIndex $ writeFileD indexhtml . renderIndex . L.reverse . L.sortOn (^._2.posted)

-- | Write a file, creating the directory containing it if necessary
writeFileD :: FilePath -> Text -> Action ()
writeFileD file content = do
    createDirectoryIfMissing True (takeDirectory file)
    writeFileUtf8 file content

genFileN :: Show a => a -> String
genFileN = Prelude.take 10 . show . run . BL.pack . show
    where run :: BL.ByteString -> Digest Whirlpool
          run = hash

pathToLink :: FilePath -> FilePath
pathToLink = joinPath . L.drop 1 . splitPath . dropExtension
