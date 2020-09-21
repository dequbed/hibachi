{-# LANGUAGE DeriveAnyClass #-}
module Data.H2O.Shake.Project
    ( writeProjectIdx
    , needProjectIdx
    , genProjectsIdx
    , addProjectReadRule
    , addProjectIndexBuildRule
    , defaultReadProject
    ) where

import qualified Prelude.ByteString as B
import qualified Prelude.ByteString.Lazy as BL

import Foreign.ForeignPtr

import Development.Shake
import Development.Shake.Rule
import Development.Shake.Classes

import Data.Conduit
import qualified Data.Conduit.Combinators as C

import Data.Binary.Put
import Data.Binary.Get

import Data.Maybe (fromJust)

import Data.H2O.Types (Project)
import Data.H2O.Project (readProject')
import Data.H2O.Shake
import Data.H2O.Shake.Branch

import Data.Tagged (untag)

import Data.Tagged (untag)

import Git hiding (writeIndex)
import Git.Types (TreeEntry(..), sourceTreeEntries)
import Git.Libgit2 (getOid, oidToSha, lgFactory)

import System.IO (print)

newtype ProjectRule = ProjectRule (Text -> Action Project)
    deriving Typeable

type instance RuleResult ProjectQ = ProjectR

data ProjectQ = ProjectQ { branch :: Text, readPath :: TreeFilePath }
    deriving (Typeable, Show, Eq, Generic, Hashable, Binary, NFData)

data ProjectA = ProjectA { readVersion :: Int, generatedCommit :: SHA, readProject :: Project }
    deriving (Typeable, Generic)
instance Binary ProjectA where
    put (ProjectA ver gen project) = do
        putInthost ver
        putByteString $ getSHA gen
        put project
    get = do
        ver <- getInthost
        gen <- SHA <$> getByteString 20
        ProjectA ver gen <$> get

data ProjectR = ProjectR Project
    deriving (Typeable, Show, Generic)
instance NFData ProjectR

readProjectRule :: (Text -> Action Project) -> Rules ()
readProjectRule act = addUserRule $ ProjectRule act

needProjects :: Text -> [TreeFilePath] -> Action [Project]
needProjects branch paths = do
    let queries = map (ProjectQ branch) paths
    r <- apply queries
    return $ map (\(ProjectR p) -> p) r

addProjectReadRule :: Rules ()
addProjectReadRule = addBuiltinRule noLint noIdentity run
  where
    run :: BuiltinRun ProjectQ ProjectR
    run key oldBin mode = do
        let old = fmap (runGet get . BL.fromStrict) oldBin
            disp _ = Just "This is another test string from ProjectRule."
            test (ProjectRule b) = Just b -- Later on filter based on ... something?
            -- I'd like to be able to actually use different kinds of text format (RST, MD, CM, TeX)
            -- but that would mean a major rewrite of the `Post` datastructure.
        (ver, act) <- getUserRuleOne key disp test

        -- Rebuild checks if the last generated commit is the most up to date one
        c <- getBranchHead $ branch key
        let oid = (getOid . untag . commitOid) c
        s <- liftIO $ withForeignPtr oid oidToSha
        if mode == RunDependenciesSame && Just ver == (readVersion <$> old) && Just s == (generatedCommit <$> old) then
            return $ RunResult ChangedNothing (fromJust oldBin) $ ProjectR (readProject $ fromJust old)
        else do
            let path = readPath key
            t <- getVersionedFile' (branch key) path
            project <- act t
            let new = BL.toStrict $ runPut $ put $ ProjectA ver s project 
            return $ RunResult ChangedRecomputeDiff new $ ProjectR project 

defaultReadProject = priority 0 $ readProjectRule (\t -> return $ readProject' t)

newtype ProjectIndexRule = ProjectIndexRule ([Project] -> Action ())
    deriving Typeable

type instance RuleResult ProjectIndexQ = ProjectIndexR

newtype ProjectIndexQ = ProjectIndexQ { fromProjectIndexQ :: [Project] }
    deriving (Typeable, Eq, Show, Generic, Hashable)

instance Binary ProjectIndexQ
instance NFData ProjectIndexQ

data ProjectIndexA = ProjectIndexA { version :: Int, fromProjectIndexA :: [Project] }
    deriving (Typeable, Generic)

instance Binary ProjectIndexA

data ProjectIndexR = ProjectIndexR
    deriving (Typeable, Show, Generic)

instance NFData ProjectIndexR

writeProjectIdx :: ([Project] -> Action ()) -> Rules ()
writeProjectIdx act = addUserRule $ ProjectIndexRule act

needProjectIdx :: [Project] -> Action ProjectIndexR
needProjectIdx projects = apply1 $ ProjectIndexQ projects

genProjectsIdx :: Text -> Rules()
genProjectsIdx branch = action $ do
    tree <- branchTree branch
    repo <- getRepo
    b <- liftIO $ withRepository lgFactory repo $
        runConduit $ sourceTreeEntries tree 
            .| C.filter (\case
                (path, BlobEntry _ _) -> ".md" `B.isSuffixOf` path
                _ -> False)
            .| C.sinkList
    p <- needProjects branch $ map fst b
    needProjectIdx p

addProjectIndexBuildRule :: Rules ()
addProjectIndexBuildRule = addBuiltinRule noLint noIdentity run
  where
    run :: BuiltinRun ProjectIndexQ ProjectIndexR
    run key oldBin mode = do
        let old = fmap (runGet get . BL.fromStrict) oldBin
            disp _ = Just "Index build rule"
            test (ProjectIndexRule b) = Just b

        (ver, act) <- getUserRuleOne key disp test
        let posts = fromProjectIndexQ key

        if mode == RunDependenciesSame && Just ver == (version <$> old) && Just posts == (fromProjectIndexA <$> old) then
            return $ RunResult ChangedNothing (fromJust oldBin) ProjectIndexR
        else do
            act posts
            let new = BL.toStrict $ runPut $ put $ ProjectIndexA ver posts
            return $ RunResult ChangedRecomputeDiff new ProjectIndexR
