module Main (main) where

import Prelude.FilePath
import Prelude.Directory
import Prelude.Text as T
import qualified Prelude.List as L
import Data.ByteString.Char8 as BL hiding (writeFile)

import Lens.Micro

import Crypto.Hash

import Data.H2O.Shake
import Data.H2O.Shake.Init
import Data.H2O.Shake.Post
import Data.H2O.Shake.Branch
import Data.H2O.Shake.Index
import Data.H2O.Shake.Tags
import Data.H2O.Post
import Data.H2O.Feed
import Data.H2O.Types

import Development.Shake

import Templates
import Style
import Path (writeFile)

-- Shake creates a declarative programming DSL for a "build system" of sorts; specific thing are
-- done based on rules that are matched based on patterns. Things to be done usually means files
-- being written, but intermediary artifacts are saved.
-- In our case we have created several rules for writing both the actual post HTML file and an index
-- of all generated post files. Both of these depend on those posts being read from git as
-- CommonMark, and share the parsed files as artifacts between each other.
-- Shake transforms the `Rules ()` into actual artifacts by sharing access to intermediary artifacts
main :: IO ()
-- `hibachiBuild` is a utility wrapper that enables all the git rules below
main = hibachiBuild $ do
    -- In the case of robots.txt we just want to copy a file from a known branch
    -- to the output
    "robots.txt" %> \out -> do
        -- getVersionedFile returns the latest blob with a given path in the
        -- given branch
        file_content <- getVersionedFile "static" "robots.txt"
        writeFile out file_content

    -- The about.html doesn't change much more often than the robots.txt but
    -- also needs a template applied
    "about.html" %> \out -> do
        file_content <- getVersionedFile "static" "about.md"
        writeFile out (aboutTemplate file_content)

    -- CSS files are generated from code and available independent of the git backend
    "css/default.css" %> \out -> writeFile out styleText
    "css/code.css" %> \out -> writeFile out styleCode

    "feed.html" %> \out ->
        writeFile out feedTemplateTxt

    "feed.xml" %> \out -> do
        posts <- needBranchPosts "posts"
        let sorted_posts = L.reverse $ L.sortOn (^._2.posted) posts
            entries = L.map (\(path,post) -> toEntry (T.pack path) post) sorted_posts
        maybe (fail "Could not generate feed") (writeFile out) $ renderFeed $ genFeed entries

    "index.html" %> \out -> do
        posts <- needBranchPosts "posts"
        writeFile out $ renderIndex $ L.reverse $ L.sortOn (^._2.posted) posts

    -- We need to tell shake to actually generate the above files.
    want ["robots.txt", "about.html", "css/default.css", "css/code.css", "feed.html", "feed.xml", "index.html"]

    -- This installs an user-defined rule. Shake will use the below code to save
    -- a post it has read from the git index.
    -- The function provided must return the path the post was saved to so links can be inserted in
    -- other pages trying to link to this post.
    writePost $ \p -> do
        let filename = genFileN $ p^.title
            path = "p" </> filename <.> "html"

        writeFile path $ renderPostText p

        return $ pathToLink path

    -- Another user-defined rule. This time to write an index file. Index files
    -- are used for the main index, tag indices and similar.
    --writeIndex $ writeFile "index.html" . renderIndex . L.reverse . L.sortOn (^._2.posted)

    writeTags $ \tagname posts -> do 
        let out = "tags" </> T.unpack tagname <.> "html"
            sorted_posts = L.reverse $ L.sortOn (^._2.posted) posts
        writeFile out $ renderTagIndex tagname sorted_posts

    -- Generate an index from all files in the `posts` branch. This will use the
    -- above defined user rule to actually figure out where to point the entries
    -- in the index.
    --wantBranchIndex "posts"

    -- Generate tag indices 
    wantTagIndex "posts"

genFileN :: Show a => a -> String
genFileN = Prelude.take 10 . show . hash' . BL.pack . show
    where hash' :: BL.ByteString -> Digest Whirlpool
          hash' = hash

pathToLink :: FilePath -> FilePath
pathToLink = joinPath . L.drop 1 . splitPath . dropExtension
