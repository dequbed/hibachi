{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (writeFile)

import Development.Shake
import Development.Shake.Rule

import Hibachi
import Hibachi.Shake
import Hibachi.Post
import Hibachi.Style

import qualified Data.Text.IO as TIO
import Data.Text.Lazy.IO (writeFile)
import Data.HashMap.Strict (empty)

main :: IO ()
main = do
    shakeArgs shakeOptions{ shakeExtra = setRepoPath "/home/glr/Documents/Blog/posts" empty } defs

defs :: Rules ()
defs = do
    setupHibachi

    want [ "out/css/default.css"
         , "out/index.html"
         , "posts"
         , "stories"
         , "projects"
         , "out/robots.txt"
        , "out/about.html"
         ]

    "out/css/default.css" %> \o -> liftIO $ writeFile o styleText

    "out/index.html" %> \p -> do
        oid <- gitRefNeed "refs/heads/posts"
        ps <- liftIO $ indexPosts Nothing "/home/glr/Documents/Blog/posts"
        liftIO $ writeFile p $ generateIndex ps

    "posts" ~> do
        oid <- gitRefNeed "refs/heads/posts"
        ps <- liftIO $ posts Nothing "/home/glr/Documents/Blog/posts"
        liftIO $ mapM_ writePost ps

    "stories" ~> do
        liftIO $ print "would build stories here"

    "projects" ~> do
        liftIO $ print "would build projects here"

    "out/robots.txt" %> \p -> do
        c <- needVersionedFile "robots.txt" "static"
        liftIO $ TIO.writeFile p c

    "out/about.html" %> \p -> do
        c <- needVersionedFile "about.md" "static"
        liftIO $ writeFile p $ hrenderText $ renderAbout c
