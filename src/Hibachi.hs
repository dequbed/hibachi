module Hibachi where

import Prelude hiding (filter, writeFile)

import Hibachi.Post
import Hibachi.Style
import Hibachi.Index

import Data.Time

import Git
import Git.Tree.Working
import Git.Libgit2 (lgFactory)

import Control.Monad.IO.Class

import Data.Tagged
import Data.Maybe
import Data.Either

import System.FilePath.Posix
import System.Directory

import Conduit
import Data.Conduit.List hiding (mapM_, mapM)

import Data.Text (Text)
import Data.Text.Lazy.IO hiding (putStrLn)
import Data.ByteString.Char8 (unpack)

import CMark
import Lucid

libmain = do
    posts "/home/glr/Documents/Blog/posts/"

posts :: FilePath -> IO ()
posts path = do
    withRepository lgFactory path $ do
        refhead <- resolveReference "refs/heads/posts"
        -- We don't do a smart check here at the moment. On optimization
        -- would be to get a list of commits since the last push and only
        -- re-generate the files that changed since then. That would also
        -- allow for "hiding" files from the index by deleting them in
        -- a 2nd commit.
        c <- lookupCommit $ Tagged $ fromJust refhead
        t <- lookupTree . commitTree $ c

        let author = signatureName $ commitAuthor c
            time   = signatureWhen $ commitCommitter c

        posts   <- rights <$> generatePosts author time t
        stories <- rights <$> generateStories author time t

        liftIO $ publishPosts posts
        liftIO $ publishStories stories
        liftIO $ publishIndex $ posts ++ stories

generatePosts :: MonadGit r m => Text -> ZonedTime -> Tree r -> m [Either ParseException Post]
generatePosts author time tree = do
    runConduit $ sourceTreeBlobEntries tree
        .| filter topLevelFile
        .| mapMC (\(p,o,_) -> do
                c <- catBlobUtf8 o
                let p2 = "p" </> (unpack p) -<.> "html"
                return (p2, c)
            )
        .| mapC (generatePost author time)
        .| sinkList

  where topLevelFile (p, _, PlainBlob) = "." == (takeDirectory $ unpack p)
        topLevelFile _                 = False

generateStories :: MonadGit r m => Text -> ZonedTime -> Tree r -> m [Either ParseException Post]
generateStories author time tree =
    runConduit $ sourceTreeBlobEntries tree
        .| filter storyFile
        .| mapMC (\(p,o,_) -> do
                c <- catBlobUtf8 o
                let p2 = "s" </> (unpack p) -<.> "html"
                return (p2, c)
            )
        .| mapC (generatePost author time)
        .| sinkList

  where storyFile (p, _, PlainBlob) = "." /= (takeDirectory $ unpack p)
        storyFile _                 = False

publishPosts :: [Post] -> IO ()
publishPosts = mapM_ publishPost
publishStories :: [Post] -> IO ()
publishStories = mapM_ publishPost
publishPost post = do
    createDirectoryIfMissing True $ takeDirectory (path post)
    writeFile (path post) $ renderText $ renderPost post

publishIndex :: [Post] -> IO ()
publishIndex = writeFile "index.html" . renderText . renderIndex
