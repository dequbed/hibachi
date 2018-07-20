module Hibachi.Git where

import Git

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

import Data.Text (Text)

import Data.Time

data PostBlob = PostBlob
    { created :: UTCTime
    , modified :: UTCTime
    , path :: TreeFilePath
    , content :: Text
    } deriving (Eq, Show)

traverseCommitsFrom :: MonadGit r m => (CommitOid r -> m a) -> Maybe (CommitOid r) -> CommitOid r -> m [a]
traverseCommitsFrom f mhave need = mapM f =<< listCommits mhave need

getPostBlobs :: MonadGit r m => Map TreeFilePath UTCTime -> Map TreeFilePath UTCTime -> Commit r -> m [PostBlob]
getPostBlobs created modified commit = do
    tree <- (lookupTree . commitTree) commit
    blobs <- treeBlobEntries tree
    mapM constructPostBlob blobs
  where
    commitTime = zonedTimeToUTC . signatureWhen . commitAuthor $ commit
    constructPostBlob (path, oid, _) = do
        c <- catBlobUtf8 oid
        return $ PostBlob createdTime modifiedTime path c
      where
        modifiedTime = M.findWithDefault commitTime path modified
        createdTime = M.findWithDefault commitTime path created

isBlob :: MonadGit r m => (TreeFilePath, TreeEntry r) -> m Bool
isBlob (_, BlobEntry _ k) = return $ k == PlainBlob
