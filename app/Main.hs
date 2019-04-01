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

    
{-
 -    "index.html" %> \p -> do
 -        idx <- withRepo "/home/glr/Documents/Blog/posts/" $ 
 -            onBranch "master" $ do
 -                posts <- getPosts
 -                stories <- getStoryPosts
 -                let combined = posts ++ stories
 -                liftAction $ need $ map path combined
 -
 -                return $ generateIndex combined
 -
 -        liftIO $ writeFile p idx
 -}

    withRepo "/home/glr/Documents/Blog/posts/" $ do
        withBranch "static" $ do
            indexMatch ["robots.txt"] $ \p -> do
                c <- gitContent p
                liftIO $ writeFile p c


    "out/css/default.css" %> \o -> liftIO $ writeFile o styleText
