{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hibachi.Shake
    where

import Git
import Git.Types
import Git.Repository
import Git.Libgit2 (lgFactory)

import Control.Exception
import Control.Monad (liftM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import Development.Shake
import Development.Shake.Rule
import Development.Shake.Classes

import Data.Text hiding (any, map)
import Data.Text.Encoding
import Data.Tagged
import Data.Maybe

import Data.Conduit
import Data.Conduit.Combinators (sinkList)
import qualified Data.Conduit.List as C

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS


-- resonable use-cases:
-- * depend on the value of a tag (blob/commit) to be able to do certain
-- action if the tag changes
-- * Depend on (branch,path) of a file in a repo
-- * depend on the value of a named reference
-- * depend on the value of a symbolic reference

newtype GitBlob = GitBlob ByteString
    deriving (Show, Eq, Hashable, Binary, NFData)
type instance RuleResult GitBlob = ByteString

data GitBlobRule = GitBlobRule GitBlob (Action ())

blobRule :: ByteString -> Action () -> Rules ()
blobRule key action = addUserRule $ GitBlobRule (GitBlob key) action

blobNeed :: ByteString -> Action ByteString
blobNeed = apply1 . GitBlob

type GitRules = ReaderT FilePath Rules
type BranchRules = ReaderT Text GitRules

withRepo = flip runReaderT
onBranch = flip runReaderT

type RepoPath = FilePath
type Branch = Text
newtype GitFile = GitFile (RepoPath, Branch, TreeFilePath)
    deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult GitFile = Text

data GitRule = GitRule GitFile (Action ())

gitRule :: (RepoPath, Branch, TreeFilePath) -> Action () -> Rules ()
gitRule key action = addUserRule $ GitRule (GitFile key) action

indexMatch :: [FilePattern] -> (FilePath -> Action ()) -> BranchRules ()
indexMatch ps f = undefined

getGitDirectoryFilesIO :: (RepoPath, Branch) -> TreeFilePath -> [FilePattern] -> IO [TreeFilePath]
getGitDirectoryFilesIO (repo, branch) dir patterns = withRepository lgFactory repo $ do
    refhead <- resolveReference $ "refs/heads/" <> branch
    c <- lookupCommit $ Tagged $ fromJust refhead
    t <- lookupTree . commitTree $ c
    e <- treeEntry t dir
    case e of
        Just (TreeEntry o) -> do
            t <- lookupTree o
            runConduit $ sourceTreeEntries t
                .| C.filter (\(_,k) -> case k of 
                    (BlobEntry _ _) -> True
                    _ -> False)
                .| C.map fst
                .| C.map (BS.unpack)
                .| C.filter (\x -> let vs = map (?==) patterns in any ($ x) vs)
                .| C.map (BS.pack)
                .| sinkList
        _ -> return []


needVersioned :: (RepoPath, Branch, TreeFilePath) -> Action Text
needVersioned = apply1 . GitFile

addBuiltinGitRule :: Rules ()
addBuiltinGitRule = addBuiltinRule noLint noIdentity run
    where
        getOid :: GitFile -> IO Text
        getOid (GitFile (repo, branch, path)) = withRepository lgFactory repo $ do
            refhead <- resolveReference $ "refs/heads/" <> branch
            c <- lookupCommit $ Tagged $ fromJust refhead
            t <- lookupTree . commitTree $ c
            e <- treeEntry t path
            return $ case e of
                Just (BlobEntry o _) -> renderObjOid o
                _ -> ""

        run :: BuiltinRun GitFile Text
        run key old mode = do
            now <- liftIO $ getOid key
            if mode == RunDependenciesSame && fmap decodeUtf8 old == Just now then
                return $ RunResult ChangedNothing (encodeUtf8 now) now
            else do
                (_, act) <- getUserRuleOne key (const Nothing) $ \(GitRule k act) -> if key == k then Just act else Nothing
                act 
                now <- liftIO $ getOid key
                return $ RunResult ChangedRecomputeDiff (encodeUtf8 now) now


gitContent = undefined
