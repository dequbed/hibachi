module Data.H2O.Shake.Index
    ( writeIndex
    , needIndex
    , addIndexBuildRule
    , needBranchPosts
    , wantBranchIndex
    ) where

import qualified Prelude.ByteString.Lazy as BL

import Development.Shake
import Development.Shake.Rule
import Development.Shake.Classes

import Data.Conduit
import qualified Data.Conduit.Combinators as C

import Data.Binary.Put
import Data.Binary.Get

import Data.Maybe (fromJust)

import Data.H2O.Types (Post)
import Data.H2O.Shake
import Data.H2O.Shake.Branch
import Data.H2O.Shake.Post

import Git hiding (writeIndex)
import Git.Types (TreeEntry(..), sourceTreeEntries)
import Git.Libgit2 (lgFactory)

import System.IO (print)

newtype IndexRule = IndexRule ([(FilePath, Post)] -> Action ())
    deriving Typeable

type instance RuleResult IndexQ = IndexR

newtype IndexQ = IndexQ { fromIndexQ :: [(FilePath, Post)] }
    deriving (Typeable, Eq, Show, Generic, Hashable)

instance Binary IndexQ
instance NFData IndexQ

data IndexA = IndexA { version :: Int, fromIndexA :: [(FilePath, Post)] }
    deriving (Typeable, Generic)

instance Binary IndexA

data IndexR = IndexR
    deriving (Typeable, Show, Generic)

instance NFData IndexR

writeIndex :: ([(FilePath, Post)] -> Action ()) -> Rules ()
writeIndex = addUserRule . IndexRule

needIndex :: [(FilePath, Post)] -> Action IndexR
needIndex = apply1 . IndexQ

needBranchPosts :: Text -> Action [(FilePath, Post)]
needBranchPosts branch = do
    tree <- branchTree branch
    repo <- getRepo
    b <- liftIO $ withRepository lgFactory repo $
        runConduit $ sourceTreeEntries tree 
            .| C.filter (\case
                (_, BlobEntry _ _) -> True
                _ -> False)
            .| C.sinkList
    p <- needPosts branch $ map fst b
    paths <- writePosts p
    return $ zip paths p

wantBranchIndex :: Text -> Rules()
wantBranchIndex branch = action $ needBranchPosts branch >>= needIndex


addIndexBuildRule :: Rules ()
addIndexBuildRule = addBuiltinRule noLint noIdentity run
  where
    run :: BuiltinRun IndexQ IndexR
    run key oldBin mode = do
        let old = fmap (runGet get . BL.fromStrict) oldBin
            disp _ = Just "Index build rule"
            test (IndexRule b) = Just b

        (ver, act) <- getUserRuleOne key disp test
        let posts = fromIndexQ key

        if mode == RunDependenciesSame && Just ver == (version <$> old) && Just posts == (fromIndexA <$> old) then
            return $ RunResult ChangedNothing (fromJust oldBin) IndexR
        else do
            act posts
            let new = BL.toStrict $ runPut $ put $ IndexA ver posts
            return $ RunResult ChangedRecomputeDiff new IndexR
