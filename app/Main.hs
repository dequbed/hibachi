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
    addBuiltinGitRule

    want ["out/css/default.css"]

    "out/robots.txt" %> \o -> do
        c <- onBranch "static" $ getGitContents o
        writeFile' o c

    "out/css/default.css" %> \o -> liftIO $ writeFile o styleText

    "out/index.html" %> \_ -> do
        posts <- onBranch "master" $ getPostsIndex
        need $ map (\p -> "out/p/" </> p -<.> ".html") posts

