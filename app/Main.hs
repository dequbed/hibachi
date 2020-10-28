module Main (main) where

import Prelude.FilePath
import Prelude.Directory
import Prelude.Text as T
import qualified Prelude.List as L
import Data.ByteString.Char8 as BL

import Lens.Micro

import Crypto.Hash

import Data.H2O.Shake
import Data.H2O.Shake.Init
import Data.H2O.Shake.Post
import Data.H2O.Shake.Branch
import Data.H2O.Shake.Index
import Data.H2O.Shake.Tags
import Data.H2O.Shake.Project
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
tagsindex :: String -> FilePath
tagsindex t = baseDir </> "tags" </> t <.> "html"
projectsindex :: FilePath
projectsindex = baseDir </> "projects/index.html"
feedhtml :: FilePath
feedhtml = baseDir </> "feed.html"
basePostDir :: FilePath
basePostDir = baseDir </> "p"

main :: IO ()
-- `hibachiBuild` is a utility wrapper that enables all the git rules below
main = hibachiBuild "/home/glr/Documents/Blog/posts" $ do
    -- The way Shake works is that we first set up rules /how/ to do a certain
    -- thing and then list what we want to be done which Shake will do by
    -- executing the defined rules (or just loading from its database)

    -- Below we define a few simple rules
    -- In the case of robots.txt we just want to copy a file from a known branch
    -- to the output
    -- `%>` is a function taking a pattern and a function that given a filename
    -- generates a file at that location. Our pattern is pretty simple - there
    -- is only one robots.txt.
    robotstxt %> \out ->
        -- getVersionedFile returns the latest blob with a given path in the
        -- given branch
        writeFileD out =<< getVersionedFile "static" "robots.txt"

    -- The about.html doesn't change much more often than the robots.txt but
    -- also needs a template applied, so we apply `aboutTemplate` over the
    -- result of `getVersionedFile` (with <$>) and return that (=<<) to
    -- `writeFileD out`.
    abouthtml %> \out ->
        writeFileD out =<< aboutTemplate <$> getVersionedFile "static" "about.md"

    -- Two more simple rules
    stylecss %> \out ->
        writeFileD out styleText
    "out/css/code.css" %> \out ->
        writeFileD out styleCode

    feedhtml %> \out ->
        writeFileD out feedTemplateTxt

    -- This rule tells Shake to generate the robots.txt, about.html and
    -- style.css files.
    want [robotstxt, abouthtml, stylecss, "out/css/code.css"]

    -- Now we get to a little bit complexer rules.
    -- With posts we don't know which ones we want to generate outside of "all
    -- of them in this git branch". So the below rule instead defines how to
    -- write a generated post to disk and returns the path where it has written
    -- that file.
    versioned 2 $ writePost (\p -> do
        let filename = genFileN $ p^.title
            path = basePostDir </> filename <.> "html"

        writeFileD path $ renderPostText p

        return $ pathToLink path)

    -- Another rule, this time to write an index file re a tag index file. Index
    -- files are used for the main index, while tag indices are used for the tag
    -- indices (duh?)
    writeIndex $ writeFileD indexhtml . renderIndex . L.reverse . L.sortOn (^._2.posted) . L.filter (^._2.indexshow)
    writeTags (\t p -> writeFileD (tagsindex $ T.unpack t) $ renderTagIndex t $ L.reverse $ L.sortOn (^._2.posted) p)
    writeProjectIdx $ writeFileD projectsindex . renderProjects . L.reverse . L.sortOn (^.projIdx)

    -- Generate an index from all files in the `posts` branch. This will use the
    -- two above defined rules to figure out where to point the links to the
    -- post entries and the latter one to actually write the index file to disk.
    genBranchIndex "posts"

    -- Like the rule above this generates an index from all posts, but this time
    -- groups them by the tags they have been tagged with. It does use the
    -- `writePost` rule to figure out where links need to be point. Since those
    -- posts have already been written by the above function it can use the
    -- stored results from that run to not have to invoke the rule twice per
    -- file.
    genTagIndex "posts"

    genProjectsIdx "projects"

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
