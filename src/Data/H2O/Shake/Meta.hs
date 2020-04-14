module Data.H2O.Shake.Meta
    ( addMetaMapRule
    , MetaMap
    , metaMap
    , metaMapCommit
    , metaMapBranch
    ) where

import qualified Prelude.Map as Map
import qualified Prelude.ByteString.Lazy as BL
import qualified Prelude.Time as T

import Foreign.ForeignPtr

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

import Data.Tagged

import Control.Monad.IO.Class
import Control.Monad.Catch
import qualified Control.Monad as M

import Conduit
import Data.Conduit
import qualified Data.Conduit.Combinators as C

import Data.H2O.Types (Meta(..), PostHeader(..), Post(..), Story(..), storyName, story)
import Data.H2O.Shake
import Data.H2O.Shake.Branch

instance NFData SHA where
    rnf a = seq a ()
instance Binary SHA where
    put = putByteString . getSHA
    get = SHA <$> getByteString 19


newtype MetaMap = MetaMap SHA
    deriving (Eq, Show, Generic, Hashable, Binary, NFData)
type instance RuleResult MetaMap = Map TreeFilePath Meta

addMetaMapRule :: Rules ()
addMetaMapRule = addBuiltinRule noLint noIdentity run
  where
    run :: BuiltinRun MetaMap (Map TreeFilePath Meta)
    run (MetaMap sha) Nothing _ = update sha Nothing Map.empty
    run (MetaMap sha) (Just stored) mode = do
        let (old, map) = decode $ BL.fromStrict stored
        if mode == RunDependenciesSame && old == sha then
            return $ RunResult ChangedNothing stored map
        else
            update sha (Just old) map

    update :: SHA -> Maybe SHA -> Map TreeFilePath Meta -> Action (RunResult (Map TreeFilePath Meta))
    update need have map = do
        repo <- getRepo
        m <- liftIO $ withRepository lgFactory repo $ do
            oneed <- Tagged <$> liftIO (shaToOid need )
            ohave <- case have of 
                Just s -> Just . Tagged <$> liftIO (shaToOid s)
                Nothing -> return Nothing
            runConduit $ sourceObjects ohave oneed False
                .| C.mapM (\(CommitObjOid c) -> return c)
                .| C.mapM lookupCommit
                .| C.foldM updateAll map
        return $ RunResult ChangedRecomputeDiff (BL.toStrict $ encode (have, m)) m

updateAll :: (MonadGit r m, HasLgRepo m, MonadMask m, MonadUnliftIO m) 
          => Map TreeFilePath Meta -> Commit -> m (Map TreeFilePath Meta)
updateAll m c = do
    let meta = metaFromCommit c
    tree <- lookupTree $ commitTree c
    runConduit $ sourceTreeEntries tree 
        .| C.foldl (update meta) m
  where 
    update :: Meta -> Map TreeFilePath Meta -> (TreeFilePath, TreeEntry) -> Map TreeFilePath Meta
    update meta map (path, BlobEntry _ _) = Map.insertWith combine path meta map
    update _ m _ = m

    -- Combining function: Author, Email & Initial posted date are kept, last modified date is
    -- updated
    combine :: Meta -> Meta -> Meta
    -- New                   Old
    combine (Meta _ _ _ modified) (Meta author email posted _) = Meta author email posted modified


metaFromCommit :: Commit -> Meta
metaFromCommit = metaFromAuthor . commitAuthor
  where
    metaFromAuthor (Signature name email when) = Meta name email (u when) (u when)
    u = T.zonedTimeToUTC

metaMap :: SHA -> Action (Map TreeFilePath Meta)
metaMap = apply1 . MetaMap

metaMapCommit :: Commit -> Action (Map TreeFilePath Meta)
metaMapCommit c = do
    let oid = (getOid . untag . commitOid) c
    s <- liftIO $ withForeignPtr oid oidToSha
    metaMap s

metaMapBranch :: BranchName -> Action (Map TreeFilePath Meta)
metaMapBranch b = do
    c <- getBranchHead b
    metaMapCommit c
