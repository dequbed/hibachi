module Data.H2O.Shake.Branch
    ( BranchName
    , addBranchHeadRule
    , branchTree
    , getBranchHead
    , getVersionedFile
    ) where

import qualified Prelude.Map as Map
import qualified Prelude.Text as T
import qualified Data.ByteString.Char8 as C

import Foreign.ForeignPtr
import GHC.Generics hiding (Meta)

import Development.Shake hiding (writeFile')
import Development.Shake.FilePath
import Development.Shake.Rule
import Development.Shake.Classes

import Git hiding (Commit, CommitOid, TreeEntry, Tree, BlobOid)
import Git.Libgit2
import Git.Libgit2.Types

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import Data.Maybe (fromJust)

import Data.Tagged

import Control.Monad.IO.Class
import Control.Monad.Catch
import qualified Control.Monad as M

import Conduit
import Data.Conduit
import qualified Data.Conduit.Combinators as C

import Data.H2O.Types (Meta(..), PostHeader(..), Post(..), Story(..), storyName, story)
import Data.H2O.Shake

instance NFData Commit where
    rnf a = seq a ()

instance Show Commit where
    show = T.unpack . renderObjOid . commitOid

type BranchName = Text
newtype Branch = Branch BranchName
    deriving (Eq, Ord, Show, Generic, Hashable, Binary, NFData)
type instance RuleResult Branch = Commit

-- | Access the latest commit in a branch as an `Action`
getBranchHead :: BranchName -> Action Commit
getBranchHead = apply1 . Branch

addBranchHeadRule :: Rules ()
addBranchHeadRule = addBuiltinRule noLint noIdentity run
  where
    run :: BuiltinRun Branch Commit
    run (Branch key) old mode = do
        repo <- getRepo
        c <- liftIO $ withRepository lgFactory repo $ do
            -- TODO: Fail more verbosly
            ref <- fromJust <$> resolveReference ("refs/heads/" <> key)
            lookupCommit (Tagged ref)

        let oid = getOid $ untag $ commitOid c
        sha <- liftIO $ withForeignPtr oid oidToSha
        let bs = getSHA sha
        if mode == RunDependenciesSame && Just bs == old then
            return $ RunResult ChangedNothing bs c
        else
            return $ RunResult ChangedRecomputeDiff bs c

-- | Get the index of a branch as an `Action`
branchTree :: BranchName -> Action Tree
branchTree branch = do
    c <- getBranchHead branch
    repo <- getRepo
    liftIO $ withRepository lgFactory repo $
        lookupTree $ commitTree c

getVersionedFile :: BranchName -> FilePath -> Action Text
getVersionedFile b p = getVersionedFile' b (C.pack p)

getVersionedFile' :: BranchName -> TreeFilePath -> Action Text
getVersionedFile' branch path = do
    t <- branchTree branch
    repo <- getRepo
    liftIO $ withRepository lgFactory repo $ 
        treeEntry t path >>= \case
            Just (BlobEntry o _) -> catBlobUtf8 o
            Just _ -> fail ("Path " <> C.unpack path <> " exists but is not a file")
            Nothing -> fail ("Path " <> C.unpack path <> " does not exist in that branch")
