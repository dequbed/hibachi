module Data.H2O.Shake 
    ( hibachiBuild
    )
    where

import qualified Prelude.List as L

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
import Control.Monad.Catch
import qualified Control.Monad as M

import Conduit
import Data.Conduit
import qualified Data.Conduit.Combinators as C

import Data.Map (Map)
import qualified Data.Map as Map

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)

import Data.Time.LocalTime
import Data.Time.Clock

import Data.Maybe
import Data.Tagged

import Data.Either.Combinators (fromRight')

import Development.Shake hiding (writeFile')
import Development.Shake.FilePath
import Development.Shake.Rule
import Development.Shake.Classes
import Git hiding (Commit, CommitOid, TreeEntry, Tree, BlobOid)
import Git.Libgit2
import Git.Libgit2.Types

import Data.H2O (Meta(..), PostHeader(..), Post(..), Story(..), storyName, story)
import Data.H2O.Read
import Data.H2O.Post (readPost, Error)

-- | Setup function that needs to be called before being able to do any other Actions requiring a
-- repository
hibachiBuild :: FilePath -> Rules () -> IO ()
hibachiBuild repo f = shakeArgs so $ do
    addBranchHeadRule
    addMetaMapRule
    f
  where
    so = shakeOptions{shakeExtra = addShakeExtra (RepoPath repo) $ shakeExtra shakeOptions}

-- | Get a List of all Posts in the index of the given branch, sorted newest to oldest
getPosts :: BranchName -> Action [Post]
getPosts = undefined

-- | Get a Map of all storied posts in the index of the given branch
getStories :: BranchName -> Action (Map Text [Post])
getStories b = do
    posts <- getPosts b
    return $ L.foldl (\map post -> case post^.story of
        Just s -> Map.insertWith (<>) (s^.storyName) [post] map
        Nothing -> map) Map.empty posts


-- | Shake option for the path of the repository
newtype RepoPath = RepoPath FilePath
getRepo = getShakeExtra >>= \case
    Just (RepoPath r) -> return r
    Nothing -> fail "Repository path isn't defined"

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
                .| C.mapM (\(CommitObjOid c) -> return c)
                .| C.mapM lookupCommit
                .| C.foldM updateAll map
        return $ RunResult ChangedRecomputeDiff (toStrict $ encode (have, m)) m

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
    u = zonedTimeToUTC

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

-- | Get the index of a branch as an `Action`
branchTree :: BranchName -> Action Tree
branchTree branch = do
    c <- getBranchHead branch
    repo <- getRepo
    liftIO $ withRepository lgFactory repo $
        lookupTree $ commitTree c

getPostsC :: BranchName -> Action (Map Text Story)
getPostsC b = do
    t <- branchTree b
    repo <- getRepo
    liftIO $ withRepository lgFactory repo $
        runConduit $ sourceTreeEntries t
            .| C.filter (\case
                (_, BlobEntry _ _) -> True
                _ -> False)
            .| C.map (\(p, BlobEntry o _) -> (p, o))
            .| C.foldM genPostMap Map.empty

genPostMap :: (MonadGit r m, HasLgRepo m, MonadMask m, MonadUnliftIO m)
           => Map Text Story -> (TreeFilePath, BlobOid) -> m (Map Text Story)
genPostMap map (path, oid) = do
    undefined


getStories' :: BranchName -> Action (Map Text (Map Int TreeFilePath))
getStories' b = do
    t <- branchTree b
    repo <- getRepo
    liftIO $ withRepository lgFactory repo $
        runConduit $ sourceTreeEntries t
            .| C.filter (\case
                (_, BlobEntry _ _) -> True
                _ -> False)
            .| C.map (\(p, BlobEntry o _) -> (p, o))
            .| C.foldM genStoryLink Map.empty


genStoryLink :: (MonadGit r m, HasLgRepo m, MonadMask m, MonadUnliftIO m) 
             => Map Text (Map Int TreeFilePath) -> (TreeFilePath, BlobOid) -> m (Map Text (Map Int TreeFilePath))
genStoryLink map (path, oid) = do
    c <- catBlobUtf8 oid
    return $ case do
            h <- readHeader c
            name <- _hdrStoryName h
            idx <- _hdrStoryIdx h
            return (name, idx)
        of
        Just (name, idx) -> Map.insertWith (<>) name (Map.singleton idx path) map
        Nothing -> map


fromBranch :: BranchName -> FilePath -> Action Text
fromBranch b p = fromBranch' b (BS.pack p)

fromBranch' :: BranchName -> TreeFilePath -> Action Text
fromBranch' branch path = do
    c <- getBranchHead branch
    repo <- getRepo
    liftIO $ withRepository lgFactory repo $ do
        t <- lookupTree $ commitTree c
        treeEntry t path >>= \case
            Just (BlobEntry o _) -> catBlobUtf8 o
            Just _ -> fail ("Path " <> BS.unpack path <> " exists but is not a file")
            Nothing -> fail ("Path " <> BS.unpack path <> " does not exist in that branch")
