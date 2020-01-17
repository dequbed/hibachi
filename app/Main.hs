module Main where

import Templates
import Style

import Data.H2O
import Data.H2O.Shake
import Data.ByteString.Char8 (unpack)
import Data.Text (Text)

out = (</>) "out"

main :: IO ()
main = hibachiBuild "/home/glr/Documents/Blog/posts" $ do
    out "robots.txt" %> \out ->
        writeFile' out =<< fromBranch "static" "robots.txt"
    out "about.html" %> \out ->
        writeFile' out =<< aboutTemplate <$> fromBranch "static" "about.md"

    out "p//*.html" %> \out ->
        writeFile' out =<< genPostPage "posts" (strip out)

    out "s/*/index.html" %> \out ->
        writeFile' out =<< genStoryPage "posts" (sstrip out)
    out "s/*/feed.xml" %> \out ->
        writeFile' out =<< genStoryRSS "posts" (sstrip out)

    out "feed.xml" %> \out ->
        writeFile' out =<< genRssFeed "posts"

    out "index.html" %> \out ->
        writeFile' out =<< genIndexPage "posts"

    out "projects.html" %> \out ->
        writeFile' out =<< genProjectsPage "projects"

    "always" ~> do
        m <- listposts "posts"
        liftIO $ print m
        need $ map (out . costrip . unpack) m 
        return ()
    --want ["robots.txt", "about.html", "feed.xml", "index.html", "projects.html"]
    want [ out "about.html" ]
    want ["always"]

strip p = (joinPath . drop 2 . splitPath) $ p -<.> "md"

costrip p = "p/" ++ p -<.> ".html"

sstrip = take 1 . drop 1 . splitPath

genPostPage = fromBranch

genRssFeed b = liftIO $ print b >> pure ""
genIndexPage b = liftIO $ print b >> pure ""
genProjectsPage b = liftIO $ print b >> pure ""
genStoryPage = undefined
genStoryRSS = undefined
