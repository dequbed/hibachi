{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hibachi.Shake
    ( gitRefNeed
    , gitResolveReference
    , setupHibachi
    , setRepoPath
    )
    where

import Development.Shake
import Development.Shake.Rule
import Development.Shake.Classes

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Maybe

import Data.Typeable (TypeRep)
import Data.Dynamic (Dynamic)
import Data.HashMap.Strict (HashMap)

import Git
import Git.Types
import Git.Libgit2 (lgFactory)

import Hibachi.Post

type RefOid = Text

newtype RepoPath = RepoPath { asPath :: FilePath }
    deriving (Show, Eq)

newtype GitRef = GitRef RefName
    deriving (Show, Eq, Hashable, Binary, NFData)
type instance RuleResult GitRef = RefOid

gitRefNeed :: RefName -> Action RefOid
gitRefNeed = apply1 . GitRef

addBuiltinGitRefRule :: Rules ()
addBuiltinGitRefRule = addBuiltinRule noLint noIdentity run
    where
        run :: BuiltinRun GitRef RefOid
        run key old mode = do
            now <- gitResolveReference key
            let nowbs = T.encodeUtf8 now
            if mode == RunDependenciesSame && old == Just nowbs then
                return $ RunResult ChangedNothing nowbs now
            else
                return $ RunResult ChangedRecomputeDiff nowbs now

gitResolveReference :: GitRef -> Action RefOid
gitResolveReference (GitRef ref) = do
    repopath <- getShakeExtra
    path <- case repopath of
        Just p -> return $ asPath p
        Nothing -> error "Repository path is not set. Did you set the path in your Shake args?"
    liftIO $ withRepository lgFactory path $ do
        refhead <- resolveReference ref
        return $ renderOid $ fromJust refhead

setupHibachi :: Rules ()
setupHibachi = do
    addBuiltinGitRefRule

setRepoPath :: FilePath -> HashMap TypeRep Dynamic -> HashMap TypeRep Dynamic
setRepoPath = addShakeExtra . RepoPath
