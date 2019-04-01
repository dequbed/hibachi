{-# LANGUAGE FlexibleContexts #-}
module Hibachi.Git
    ( getModifiedEntries
    )
    where

import Git
import Git.Types
import Git.Tree.Working

import Data.HashMap.Lazy

getParentTrees :: MonadGit r m => Commit r -> m [Tree r]
getParentTrees c = do
    parents <- lookupCommitParents c
    mapM (lookupTree . commitTree) parents

-- Returns a List that contains only those elements of the second Tree that
-- are in none of the first Trees or have been changed (Based on the Eq
-- instance of TreeEntry r) since
listChangedEntries :: (MonadGit r m, Eq (TreeEntry r)) => [Tree r] -> Tree r -> m [(TreeFilePath, TreeEntry r)]
listChangedEntries parents tree = do
    pentries <- mapM listTreeEntries parents
    let pentries' = concat pentries
    tentries <- listTreeEntries tree
    return $ Prelude.filter (`notElem` pentries') tentries

getModifiedEntries :: (MonadGit r m, Eq (TreeEntry r)) => Commit r -> m [(TreeFilePath, TreeEntry r)]
getModifiedEntries commit = do
    parenttrees <- getParentTrees commit
    committree <- (lookupTree . commitTree) commit
    listChangedEntries parenttrees committree

getModifiedEntriesList :: (MonadGit r m, Eq (TreeEntry r)) => [Commit r] -> m [(TreeFilePath, TreeEntry r)]
getModifiedEntriesList commits = 
    (toList . fromList . concat) -- fromList/toList are from HashMap which will remove duplicates
    <$> mapM getModifiedEntries commits
