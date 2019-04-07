{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hibachi.Shake
    ( gitRefNeed
    , gitResolveReference
    , setupHibachi
    , setRepoPath
    , getRepoPath
    , gitFileOid
    , gitCatFile
    , needVersionedFile
    )
    where

import Development.Shake
import Development.Shake.Rule
import Development.Shake.Classes

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Maybe

import qualified Data.ByteString.Char8 as B

import Data.Tagged
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
gitResolveReference (GitRef ref) = withOurRepository $ do
    refhead <- resolveReference ref
    return $ renderOid $ fromJust refhead

newtype GitFile = GitFile (FilePath, Text) 
    deriving (Show, Eq, Hashable, Binary, NFData)
type instance RuleResult GitFile = Text

needVersionedFile :: FilePath -> Text -> Action Text
needVersionedFile a b = apply1 $ curry GitFile a b

addBuiltinGitFileRule :: Rules ()
addBuiltinGitFileRule = addBuiltinRule noLint noIdentity run
    where
        run :: BuiltinRun GitFile Text
        run key old mode = do
            now <- gitFileOid key
            content <- gitCatFile now
            let now' = T.encodeUtf8 now
            if mode == RunDependenciesSame && old == Just now' then
                return $ RunResult ChangedNothing now' content
            else
                return $ RunResult ChangedRecomputeDiff now' content

gitFileOid :: GitFile -> Action Text
gitFileOid (GitFile (path, branch)) = withOurRepository $ do
    refhead <- resolveReference $ "refs/heads/" <> branch
    c <- case refhead of
        Just p -> lookupCommit $ Tagged $ p
        Nothing -> error $ "Branch " ++ T.unpack branch ++ "does not exist."
    e <- commitTreeEntry c (B.pack path)
    case e of
        Just (BlobEntry o _) -> return $ renderObjOid o
        Just _ -> error $ "Object " ++ path ++ " on branch " ++ T.unpack branch ++ " isn't a file."
        Nothing -> error $ "Object " ++ path ++ " on branch " ++ T.unpack branch ++ " doesn't exist."

gitCatFile :: Text -> Action Text
gitCatFile oid = withOurRepository $ catBlobUtf8 =<< parseObjOid oid

setupHibachi :: Rules ()
setupHibachi = do
    addBuiltinGitRefRule
    addBuiltinGitFileRule

setRepoPath :: FilePath -> HashMap TypeRep Dynamic -> HashMap TypeRep Dynamic
setRepoPath = addShakeExtra . RepoPath

getRepoPath :: Action FilePath
getRepoPath = do
    repopath <- getShakeExtra
    case repopath of
        Just p -> return $ asPath p
        Nothing -> error "Repository path is not set. Did you set the path in your Shake args?"

withOurRepository f = do
    repopath <- getRepoPath
    liftIO $ withRepository lgFactory repopath f
