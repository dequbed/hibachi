{-# LANGUAGE FlexibleContexts #-}
module Hibachi.Git
    ( getModifiedEntries
    , getModifiedEntriesList
    , getGitContents
    , generateFromCommits
    , toPost
    )
    where

import Control.Lens
import Control.Monad (filterM)

import Data.Maybe

import Data.ByteString.Char8 (unpack)

import Git
import Git.Types
import Git.Tree.Working

import Hibachi.Post

import qualified Data.Map.Lazy as Map

getParentTrees :: MonadGit r m => Commit r -> m [Tree r]
getParentTrees c = do
    parents <- lookupCommitParents c
    mapM (lookupTree . commitTree) parents

-- Returns a List that contains only those elements of the second Tree that
-- are in none of the first Trees or have been changed (Based on the Eq
-- instance of TreeEntry r) since
listChangedEntries :: MonadGit r m => [Tree r] -> Tree r -> m [(TreeFilePath, TreeEntry r)]
listChangedEntries parents tree = do
    pentries <- mapM listTreeEntries parents
    let pentries' = concat pentries
    tentries <- listTreeEntries tree
    filterM (notInParents pentries') tentries

notInParents :: MonadGit r m => [(TreeFilePath, TreeEntry r)] -> (TreeFilePath, TreeEntry r) -> m Bool
notInParents ps (path, (BlobEntry o _)) = do
    let map = Map.fromList ps

    return $ case Map.lookup path map of
        Nothing -> True
        Just (BlobEntry o2 _) -> o /= o2
        otherwise -> False
notInParents _ _ = return $ False

getModifiedEntries :: MonadGit r m => Commit r -> m [(TreeFilePath, TreeEntry r)]
getModifiedEntries commit = do
    parenttrees <- getParentTrees commit
    committree <- (lookupTree . commitTree) commit
    listChangedEntries parenttrees committree

getModifiedEntriesList :: MonadGit r m => [Commit r] -> m [(Commit r, [(TreeFilePath, TreeEntry r)])]
getModifiedEntriesList commits = mapM (\c -> do
         e <- getModifiedEntries c
         return (c, e)) commits

toModifiedPosts :: MonadGit r m => (Commit r, [(TreeFilePath, TreeEntry r)]) -> m (Commit r, [Maybe Post])
toModifiedPosts a@(c, _) = mapMOf _2 (mapM (toPost c)) a


toModifiedPostsList :: MonadGit r m => [(Commit r, [(TreeFilePath, TreeEntry r)])] -> m [(Commit r, [Post])]
toModifiedPostsList x = do
    a <- mapM toModifiedPosts x
    return $ map (f) a
    where
        f :: (Commit r, [Maybe Post]) -> (Commit r, [Post])
        f (c, ps) = (c, catMaybes ps)

getModifiedPostsList :: MonadGit r m => [Commit r] -> m [(Commit r, [Post])]
getModifiedPostsList cs = toModifiedPostsList =<< (getModifiedEntriesList cs)

toPost :: MonadGit r m => Commit r -> (TreeFilePath, TreeEntry r) -> m (Maybe Post)
toPost c (path, entry) = do
    let author = signatureName $ commitAuthor c
        time   = signatureWhen $ commitCommitter c

    case entry of
        (BlobEntry oid _) -> do
            rawcontent <- catBlobUtf8 oid
            case generatePost author time path rawcontent of
                Left _ -> return Nothing
                Right p -> return $ Just p
        _ -> return Nothing

generateFromCommits :: MonadGit r m => [Commit r] -> m [[Post]]
generateFromCommits a = map snd <$> getModifiedPostsList a

getGitContents :: String -> FilePath -> IO String
getGitContents branch path = do
    return ""
