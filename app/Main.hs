{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Prelude                hiding (writeFile, (*>))

import           Development.Shake
import           Development.Shake.Rule

import           System.Directory
import           System.FilePath.Posix

import           Hibachi
import           Hibachi.Post
import           Hibachi.Style
import           Hibachi.Templates
import           Hibachi.Util

import           Data.HashMap.Strict    (empty)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.IO      (writeFile)
import qualified Data.Text.Lazy.IO      as TLIO

main :: IO ()
main = do
    shakeArgs shakeOptions
        { shakeExtra = setRepoPath "/home/glr/Documents/Blog/posts"
            empty
        , shakeVersion = "0.1.0.0"
        } defs

defs :: Rules ()
defs = versioned 1 $ do
    setupHibachi

    want [ "out/css/default.css"
         , "out/css/code.css"
         , "out/index.html"
         , "posts"
         --, "stories"
         --, "out/projects.html"
         , "out/robots.txt"
         , "out/about.html"
         ]

    "out/css/default.css" %> \p -> writeFileTL p styleText
    "out/css/code.css" %> \p -> writeFile' p styleCode

    "out/index.html" %> \p -> do
        oid <- gitRefNeed "refs/heads/posts"
        ps <- buildAllAction oid
        writeFileTL p $ hrenderText $ renderPostIndex ps

    "posts" ~> do
        oid <- gitRefNeed "refs/heads/posts"
        ps <- buildAllAction oid
        mapM_ writePost ps


    "out/robots.txt" %> \p ->
        writeFileText p =<< needVersionedFile "static" "robots.txt"

    "out/about.html" %> \p ->
        writeFileHtml p <$> renderAbout =<< needVersionedFile "static" "about.md"

    -- Using runAfter, save the last generated commit for the next time the
    -- generator is run.
    -- runAfter getHEADCommitAndSave

writeFileText :: FilePath -> T.Text  -> Action ()
writeFileText path content = liftIO $ do
    createDirectoryIfMissing True $ takeDirectory path
    TIO.writeFile path content
writeFileTL   :: FilePath -> TL.Text -> Action ()
writeFileTL   path content = liftIO $ do
    createDirectoryIfMissing True $ takeDirectory path
    TLIO.writeFile path content
writeFileHtml :: FilePath -> Html () -> Action ()
writeFileHtml path content = liftIO $ do
    createDirectoryIfMissing True $ takeDirectory path
    TLIO.writeFile path $ hrenderText content

writePost :: Post -> Action ()
writePost p@(PlainPost (PostCommon{postLinkPath=path})) = inner path p
writePost p@(Story _ (PostCommon{postLinkPath=path}) _) = inner path p
inner path p = writeFileTL ("out" </> path) $ hrenderText $ renderPost p
