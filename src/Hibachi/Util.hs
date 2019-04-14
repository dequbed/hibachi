module Hibachi.Util
    ( History
    , buildHistory
    , CommitMeta(..)
    , buildAll
    , hrenderText
    )
    where

import Control.Monad.Catch (MonadMask)

import Prelude hiding (filter, writeFile)

import Hibachi.Post
import Hibachi.Style
import Hibachi.Index
import Hibachi.Git

import Data.Time

import Git
import Git.Tree.Working
import Git.Libgit2 (lgFactory, LgRepo, HasLgRepo)

import Control.Monad
import Control.Monad.IO.Class

import Data.Tagged
import Data.Maybe
import Data.Either
import Data.List

import System.FilePath.Posix
import System.Directory

import Conduit
import qualified Data.Conduit.Combinators as CC

import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.ByteString.Char8 (unpack, pack)
import qualified Data.Text.Lazy.IO as TIO

import qualified Data.Map.Lazy as Map

import CMark
import Lucid

type CommitMeta = (Text, ZonedTime)
type Latest = Map.Map TreeFilePath (CommitMeta, Text)
insertLatestModified :: TreeFilePath -> (CommitMeta, Text) -> Latest -> Latest
insertLatestModified = Map.insertWith const
type First = Map.Map TreeFilePath CommitMeta
insertFirstModified :: TreeFilePath -> CommitMeta -> First -> First
insertFirstModified = Map.insertWith seq

data History = History 
             { firstPostedMap :: First
             , latestUpdateMap :: Latest
             } deriving (Eq, Show)

empty :: History
empty = History Map.empty Map.empty

buildHistory :: MonadGit r m => Maybe (History, CommitOid r) -> CommitOid r -> m History
buildHistory (Just (hist, have)) need =
    foldM buildHistoryStep hist =<< mapM lookupCommit =<< listCommits (Just have) need
buildHistory Nothing need =
    foldM buildHistoryStep empty =<< mapM lookupCommit =<< listCommits Nothing need

buildHistoryStep :: MonadGit r m => History -> Commit r -> m History
buildHistoryStep h commit = do
    tree <- lookupTree $ commitTree commit
    runConduit $ sourceTreeBlobEntries tree
        .| CC.foldM (updateHistory (author, ctime)) h
  where
    ctime = signatureWhen $ commitCommitter commit
    author = signatureName $ commitAuthor commit
    updateHistory :: MonadGit r m => CommitMeta -> History -> (TreeFilePath, BlobOid r, BlobKind) -> m History
    updateHistory meta (History f l) (path, oid, _) = do
        let toid = renderObjOid oid
        let l' = insertLatestModified path (meta, toid) l
            f' = insertFirstModified path meta f
        return $ History f' l'

buildAll :: (MonadGit r m, MonadIO m, MonadThrow m, MonadMask m, MonadUnliftIO m, HasLgRepo m) => History -> Tree r -> m [Post]
buildAll hist tree = do
    storytrees <- runConduit $ sourceTreeEntries tree
        .| CC.filter (\e -> case e of (_, (TreeEntry _)) -> True ; otherwise -> False)
        .| CC.map (\e -> case e of (p, (TreeEntry o)) -> (p, o))
        .| CC.mapM (\(p,o) -> (,) p <$> lookupTree o)
        .| sinkList
    posts <- runConduit $ sourceTreeBlobEntries tree
        .| CC.filter (\(p, _, _) -> takeDirectory (unpack p) == ".")
        .| sinkList

    ps <- mapM (buildPost hist <$> (\(a,_,_) -> a)) posts
    ss <- mapM (uncurry (buildStories hist)) storytrees
    return $ sortPostByDate $ (rights ps) ++ (concat ss)

buildPost :: (MonadGit r m, MonadThrow m, MonadMask m, MonadUnliftIO m, HasLgRepo m) => History -> TreeFilePath -> m (Either PostError Post)
buildPost h p = do
    c <- buildCommon h p
    return $ PlainPost <$> c

buildCommon :: (MonadGit r m, MonadThrow m, MonadMask m, MonadUnliftIO m, HasLgRepo m) => History -> TreeFilePath -> m (Either PostError PostCommon)
buildCommon (History f l) path = do
    let (author, postedTime) = f Map.! path
        (_, toid) = l Map.! path
    oid <- parseObjOid toid
    content <- catBlobUtf8 oid
    return $ generateCommon author postedTime path content

buildStories :: (MonadGit r m, MonadThrow m, MonadMask m, MonadUnliftIO m, HasLgRepo m) => History -> TreeFilePath -> Tree r -> m [Post]
buildStories hist prefix tree = do
    (path, _, _) <- unzip3 <$> treeBlobEntries tree
    stories <- mapM (buildCommon hist) $ map (\path -> pack $ (unpack $ prefix) </> (unpack $ path)) path
    return $ walkStories $ rights stories

walkStories :: [PostCommon] -> [Post]
walkStories xs = walkStories' [] xs []

walkStories' :: [PostCommon] -> [PostCommon] -> [Post] -> [Post]
walkStories' _    []          a = a
walkStories' prev (this:next) a = walkStories' (prev ++ [this]) next (a ++ [Story prev this next])

sortPostByDate :: [Post] -> [Post]
sortPostByDate = reverse . sortOn postedTime
  where postedTime (PlainPost common) = postPostedTime common
        postedTime (Story _ common _) = postPostedTime common
sortByDate :: [PostCommon] -> [PostCommon]
sortByDate = reverse . sortOn postPostedTime

hrenderText :: Html () -> TL.Text
hrenderText = renderText
