module Hibachi where

import Prelude hiding (filter, writeFile)

import Hibachi.Post
import Hibachi.Style
import Hibachi.Index
import Hibachi.Shake
import Hibachi.Git

import Data.Time

import Git
import Git.Tree.Working
import Git.Libgit2 (lgFactory)

import Control.Monad.IO.Class

import Data.Tagged
import Data.Maybe
import Data.Either
import Data.List

import System.FilePath.Posix
import System.Directory

import Conduit
import Data.Conduit.List hiding (mapM_, mapM)

import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.ByteString.Char8 (unpack)
import qualified Data.Text.Lazy.IO as TIO

import qualified Data.Map.Lazy as Map

import CMark
import Lucid

libmain = do
    posts Nothing "/home/glr/Documents/Blog/posts/"

posts :: Maybe Text -> FilePath -> IO [Post]
posts lastcommit path = do
    withRepository lgFactory path $ do
        refhead <- resolveReference "refs/heads/posts"
        lc <- case lastcommit of
            Just t -> do
                o <- parseOid t
                return $ Just $ Tagged o
            Nothing -> return $ Nothing

        let thead = Tagged $ fromJust refhead
        os <- listCommits lc thead
        cs <- mapM lookupCommit os

        Prelude.map adjustPath <$> Prelude.concat <$> generateFromCommits cs

indexPosts :: Maybe Text -> FilePath -> IO [Post]
indexPosts lastcommit path = withRepository lgFactory path $ do
        refhead <- resolveReference "refs/heads/posts"
        let thead = Tagged $ fromJust refhead
        c <- lookupCommit thead
        t <- lookupTree $ commitTree c
        indexentries <- Prelude.map (\(a,_,_) -> a) <$> treeBlobEntries t

        lc <- case lastcommit of
            Just t -> do
                o <- parseOid t
                return $ Just $ Tagged o
            Nothing -> return $ Nothing

        os <- listCommits lc thead
        cs <- mapM lookupCommit os

        allentries <- getModifiedEntriesList cs

        liftIO $ print $ length allentries
        liftIO $ print $ indexentries

        found <- mapM (findWhereModified (reverse allentries)) $ indexentries
        posts <- Data.Maybe.catMaybes <$> mapM (uncurry toPost) found

        return $ Prelude.map adjustPath $ sortByDate posts

findWhereModified :: MonadGit r m => [(Commit r, [(TreeFilePath, TreeEntry r)])] -> TreeFilePath -> m (Commit r, (TreeFilePath, TreeEntry r)) 
findWhereModified ((c, tes):aystack) needle = let map = Map.fromList tes in
    case Map.lookup needle map of
        Just entry -> return $ (c, (needle, entry))
        Nothing -> findWhereModified aystack needle

sortByDate :: [Post] -> [Post]
sortByDate = reverse . sortOn posted

generateIndex :: [Post] -> TL.Text
generateIndex = renderText . renderIndex

writePost :: Post -> IO ()
writePost p@(Post{path=path}) = do
    let outpath = "out" </> dropDrive path
        outdir = takeDirectory outpath
    createDirectoryIfMissing True outdir
    TIO.writeFile outpath $ renderText $ renderPost p
    return ()

adjustPath :: Post -> Post
adjustPath p@(Post {path=path}) = let outpath = "/p/" </> path -<.> "html" in
    p {path=outpath}

