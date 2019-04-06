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

main :: IO ()
main = shakeArgs shakeOptions $ do
    want ["out/css/default.css", "out/index.html", "posts"]

    "out/css/default.css" %> \o -> liftIO $ writeFile o styleText

    "out/index.html" %> \p -> do
        ps <- liftIO $ indexPosts Nothing "/home/glr/Documents/Blog/posts"
        liftIO $ writeFile p $ generateIndex ps
    "posts" ~> do
        ps <- liftIO $ posts Nothing "/home/glr/Documents/Blog/posts"
        liftIO $ mapM_ writePost ps
