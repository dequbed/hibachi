module Data.H2O.Shake 
    ( module Development.Shake
    , module Development.Shake.FilePath
    , hibachiBuild
    , writeFile'
    , fromBranch
    , branchNeed
    , lastMod
    , lastModC
    , lastModB
    , listposts
    )
    where

import Prelude hiding (mapM, foldl)

import Foreign.ForeignPtr
import GHC.Generics hiding (Meta)

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Monad.IO.Class

import Conduit
import Data.Conduit
import Data.Conduit.Combinators as C

import Data.Map (Map)
import qualified Data.Map as Map

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)

import Data.Time.LocalTime
import Data.Time.Clock

import Data.Maybe
import Data.Tagged

import Development.Shake hiding (writeFile')
import Development.Shake.FilePath
import Development.Shake.Rule
import Development.Shake.Classes
import Git hiding (Commit, CommitOid, TreeEntry, Tree, BlobOid)
import Git.Libgit2
import Git.Libgit2.Types

import Control.Monad.Catch

import Data.H2O (Meta(..), PostHeader(..))
import Data.H2O.Read

instance NFData Commit where
    rnf a = seq a ()

instance Show Commit where
    show = T.unpack . renderObjOid . commitOid

type BranchName = Text
newtype Branch = Branch BranchName
    deriving (Eq, Ord, Show, Generic, Hashable, Binary, NFData)
type instance RuleResult Branch = Commit

branchNeed :: BranchName -> Action Commit
branchNeed = apply1 . Branch

newtype RepoPath = RepoPath FilePath

addBuiltinBranchRule :: Rules ()
addBuiltinBranchRule = addBuiltinRule noLint noIdentity run
  where
    run :: BuiltinRun Branch Commit
    run (Branch key) old mode = do
        repo <- getRepo
        c <- liftIO $ withRepository lgFactory repo $ do
            ref <- fromJust <$> resolveReference ("refs/heads/" <> key)
            lookupCommit (Tagged ref)

        let oid = getOid $ untag $ commitOid c
        sha <- liftIO $ withForeignPtr oid oidToSha
        let bs = getSHA sha
        if mode == RunDependenciesSame && Just bs == old then
            return $ RunResult ChangedNothing bs c
        else
            return $ RunResult ChangedRecomputeDiff bs c

instance NFData SHA where
    rnf a = seq a ()
instance Binary SHA where
    put = putByteString . getSHA
    get = SHA <$> getByteString 19

newtype LastModified = LastModified SHA
    deriving (Eq, Show, Generic, Hashable, Binary, NFData)
type instance RuleResult LastModified = Map TreeFilePath Meta

addBuiltinModifiedRule :: Rules ()
addBuiltinModifiedRule = addBuiltinRule noLint noIdentity run
  where
    run :: BuiltinRun LastModified (Map TreeFilePath Meta)
    run (LastModified sha) Nothing _ = update sha Nothing Map.empty
    run (LastModified sha) (Just stored) mode = do
        let (old, map) = decode $ fromStrict stored
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
                .| mapM (\(CommitObjOid c) -> return c)
                .| mapM lookupCommit
                .| foldM updateAll map
        return $ RunResult ChangedRecomputeDiff (toStrict $ encode (have, m)) m

updateAll :: (MonadGit r m, HasLgRepo m, MonadMask m, MonadUnliftIO m) 
          => Map TreeFilePath Meta -> Commit -> m (Map TreeFilePath Meta)
updateAll m c = do
    let meta = metaFromCommit c
    tree <- lookupTree $ commitTree c
    runConduit $ sourceTreeEntries tree 
        .| foldl (updt meta) m
  where 
    updt :: Meta -> Map TreeFilePath Meta -> (TreeFilePath, TreeEntry) -> Map TreeFilePath Meta
    updt meta map (path, BlobEntry _ _) = Map.insertWith f path meta map
    updt _ m _ = m

    f :: Meta -> Meta -> Meta
    f (Meta _ _ _ m) (Meta a e p _) = Meta a e p m

metaFromCommit :: Commit -> Meta
metaFromCommit = metaFromAuthor . commitAuthor
  where
    metaFromAuthor (Signature name email when) = Meta name email (u when) (u when)
    u = zonedTimeToUTC

lastMod :: SHA -> Action (Map TreeFilePath Meta)
lastMod = apply1 . LastModified

lastModC :: Commit -> Action (Map TreeFilePath Meta)
lastModC c = do
    let oid = (getOid . untag . commitOid) c
    s <- liftIO $ withForeignPtr oid oidToSha
    lastMod s

lastModB :: BranchName -> Action (Map TreeFilePath Meta)
lastModB b = do
    c <- branchNeed b
    lastModC c

needBTree :: BranchName -> Action Tree
needBTree branch = do
    c <- branchNeed branch
    repo <- getRepo
    liftIO $ withRepository lgFactory repo $
        lookupTree $ commitTree c

listposts :: BranchName -> Action [TreeFilePath]
listposts b = do
    t <- needBTree b
    repo <- getRepo
    liftIO $ withRepository lgFactory repo $
        runConduit $ sourceTreeEntries t
            .| C.filter (\case
                (_, BlobEntry _ _) -> True
                _ -> False)
            .| C.map fst
            .| sinkList

getStories :: BranchName -> Action (Map Text [(Int, TreeFilePath)])
getStories b = do
    t <- needBTree b
    repo <- getRepo
    liftIO $ withRepository lgFactory repo $
        runConduit $ sourceTreeEntries t
            .| C.filter (\case
                (_, BlobEntry _ _) -> True
                _ -> False)
            .| C.map (\(p, BlobEntry o _) -> (p, o))
            .| foldM genStoryLink Map.empty


genStoryLink :: (MonadGit r m, HasLgRepo m, MonadMask m, MonadUnliftIO m) 
             => Map Text [(Int, TreeFilePath)] -> (TreeFilePath, BlobOid) -> m (Map Text [(Int, TreeFilePath)])
genStoryLink map (path, oid) = do
    c <- catBlob oid
    return $ case do
            h <- readHeader c
            name <- _hdrStoryName h
            idx <- _hdrStoryIdx h
            return (name, idx)
        of
        Just (name, idx) -> Map.insertWith (<>) name [(idx, path)] map
        Nothing -> map


hibachiBuild :: FilePath -> Rules () -> IO ()
hibachiBuild repo f = shakeArgs so $ do
    addBuiltinBranchRule
    addBuiltinModifiedRule
    f
  where
    so = shakeOptions{shakeExtra = addShakeExtra (RepoPath repo) $ shakeExtra shakeOptions}

writeFile' :: MonadIO m => FilePath -> Text -> m ()
writeFile' p x = liftIO $ TIO.writeFile p x

fromBranch :: BranchName -> FilePath -> Action Text
fromBranch branch path = do
    c <- branchNeed branch
    repo <- getRepo
    liftIO $ withRepository lgFactory repo $ do
        t <- lookupTree $ commitTree c
        treeEntry t (BS.pack path) >>= \case
            Just (BlobEntry o _) -> catBlobUtf8 o
            Just _ -> fail ("Path " <> path <> " exists but is not a file")
            Nothing -> fail ("Path " <> path <> " does not exist in that branch")

getRepo = getShakeExtra >>= \case
    Just (RepoPath r) -> return r
    Nothing -> fail "Repository path isn't defined"
