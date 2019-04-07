{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (writeFile)

import Development.Shake
import Development.Shake.Rule

import Hibachi
import Hibachi.Shake
import Hibachi.Post
import Hibachi.Style

import Data.Text.Lazy.IO (writeFile)
import Data.HashMap.Strict (empty)

main :: IO ()
main = do
    shakeArgs shakeOptions{ shakeExtra = setRepoPath "/home/glr/Documents/Blog/posts" empty } defs

defs :: Rules ()
defs = do
    setupHibachi

    want ["out/css/default.css", "out/index.html", "posts"]

    "out/css/default.css" %> \o -> liftIO $ writeFile o styleText

    "out/index.html" %> \p -> do
        oid <- gitRefNeed "refs/heads/posts"
        ps <- liftIO $ indexPosts Nothing "/home/glr/Documents/Blog/posts"
        liftIO $ writeFile p $ generateIndex ps

    "posts" ~> do
        oid <- gitRefNeed "refs/heads/posts"
        ps <- liftIO $ posts Nothing "/home/glr/Documents/Blog/posts"
        liftIO $ mapM_ writePost ps
