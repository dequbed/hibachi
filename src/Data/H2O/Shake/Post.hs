{-# LANGUAGE DeriveAnyClass #-}
module Data.H2O.Shake.Post
    ( writePost
    , addPostBuildRule
    , readPostr
    , addPostReadRule
    , defaultReadPost
    , wantPosts
    , wantPostsBranch
    , needPosts
    , writePosts
    ) where

import qualified Prelude.List as L
import qualified Prelude.Map as Map
import qualified Prelude.Map.Partial as Map
import qualified Prelude.ByteString as B
import qualified Prelude.ByteString.Lazy as BL

import System.IO (print, putStrLn)

import Foreign.ForeignPtr

import Development.Shake
import Development.Shake.Rule
import Development.Shake.Classes

import Data.Conduit
import qualified Data.Conduit.Combinators as C

import Data.Binary.Put
import Data.Binary.Get

import Data.H2O.Types (Post(..), story, storyName, Meta)
import Data.H2O.Shake
import Data.H2O.Shake.Meta (metaMap)
import Data.H2O.Shake.Branch
import Data.H2O.Post (readPost')

import Data.Maybe (fromJust)

import Data.Tagged (untag)

import Git.Types (SHA(..), TreeFilePath, commitOid, TreeEntry(..), sourceTreeEntries)
import Git.Repository
import Git.Libgit2 (getOid, oidToSha, lgFactory)
import Git.Libgit2.Types (Commit(..), CommitOid(..), TreeOid)

--deriving instance Generic SHA
--instance Binary SHA
--instance NFData SHA

newtype PostRule = PostRule (Post -> Action FilePath)
    deriving Typeable

type instance RuleResult PostQ = PostR

newtype PostQ = PostQ { fromPostQ :: Post }
    deriving (Typeable, Eq, Show, Generic, Hashable)

instance Binary PostQ

instance NFData PostQ

data PostA = PostA { fromPostA :: Post, version :: Int, path :: FilePath }
    deriving (Typeable, Generic)

deriving instance Binary PostA

newtype PostR = PostR FilePath
    deriving (Typeable, Show, Generic)

deriving instance NFData PostR

writePosts :: [Post] -> Action [FilePath]
writePosts posts = do
    paths <- apply $ map PostQ posts
    return $ map (\(PostR path) -> path) paths

-- | Define a rule how to write a post.
--   H2O knows *what* posts do build but not /how/. This is the way for an user to tell it.
--   The rule needs to return the path where the post was written so link consistency can be
--   enforced
writePost :: (Post -> Action FilePath) -> Rules ()
writePost act = addUserRule $ PostRule act

-- | Add the rule systems to run the user rule defined above
addPostBuildRule :: Rules ()
addPostBuildRule = addBuiltinRule noLint noIdentity run
  where
    run :: BuiltinRun PostQ PostR
    run key oldBin mode = do
        let old = fmap (runGet get . BL.fromStrict) oldBin
            disp _ = Just "This is a test string because I'm not sure what to put here. It's called `disp`."
            test (PostRule b) = Just b
        -- Get the user-defined `writePost` rule
        (ver, act) <- getUserRuleOne key disp test
        -- Use that rule to actually write the post
        let post = fromPostQ key
        -- True if all rule deps are the same, the rule has run before and from that run the rule
        -- version is the same and the OID of the post file is the same.
        if mode == RunDependenciesSame && Just ver == (version <$> old) && Just post == (fromPostA <$> old) then
            return $ RunResult ChangedNothing (fromJust oldBin) $ PostR (path $ fromJust old)
        else do
            -- Only if something changed actually evaluate the user rule and generate a post.
            path <- act post
            let new = BL.toStrict $ runPut $ put $ PostA post ver path
            return $ RunResult ChangedRecomputeDiff new $ PostR path

-- Two rules: WritePost that writes a Post, returns the path it was written to (important for url
-- resolution). ReadPost that takes a TreeFilePath and generates a Post from that (so that users can
-- use TeX or Markdown or RST instead of commonmark).

newtype ReadPostRule = ReadPostRule (Meta -> Text -> Action Post)
    deriving Typeable

type instance RuleResult ReadPostQ = ReadPostR

data ReadPostQ = ReadPostQ { branch :: Text, readPath :: TreeFilePath }
    deriving (Typeable, Show, Eq, Generic, Hashable, Binary, NFData)

data ReadPostA = ReadPostA { readVersion :: Int, generatedCommit :: SHA, readPost :: Post }
    deriving (Generic)

instance Binary ReadPostA where
    put (ReadPostA ver gen post) = do
        putInthost ver
        putByteString $ getSHA gen
        put post
    get = do
        ver <- getInthost
        -- A SHA1 hash is always exactly 160 bit or 20 byte long.
        gen <- SHA <$> getByteString 20
        ReadPostA ver gen <$> get

newtype ReadPostR = ReadPostR Post
    deriving (Typeable, Show, Generic, NFData)

readPostr :: (Meta -> Text -> Action Post) -> Rules()
readPostr act = addUserRule $ ReadPostRule act

needPosts :: Text -> [TreeFilePath] -> Action [Post]
needPosts branch paths = do
    let queries = map (ReadPostQ branch) paths
    r <- apply queries
    return $ map (\(ReadPostR p) -> p) r

wantPosts :: Text -> [TreeFilePath] -> Rules ()
wantPosts branch paths = action $ do
    p <- needPosts branch paths
    writePosts p
    return ()

wantPostsBranch :: Text -> Rules ()
wantPostsBranch branch = action $ do
    tree <- branchTree branch
    repo <- getRepo
    b <- liftIO $ withRepository lgFactory repo $
        runConduit $ sourceTreeEntries tree 
            .| C.filter (\case
                (_, BlobEntry _ _) -> True
                _ -> False)
            .| C.sinkList
    p <- needPosts branch $ map fst b
    writePosts p


addPostReadRule :: Rules ()
addPostReadRule = addBuiltinRule noLint noIdentity run
  where
    run :: BuiltinRun ReadPostQ ReadPostR
    run key oldBin mode = do
        let old = fmap (runGet get . BL.fromStrict) oldBin
            disp _ = Just "This is another test string from PostReadRule."
            test (ReadPostRule b) = Just b -- Later on filter based on ... something?
            -- I'd like to be able to actually use different kinds of text format (RST, MD, CM, TeX)
            -- but that would mean a major rewrite of the `Post` datastructure.
        (ver, act) <- getUserRuleOne key disp test

        -- Rebuild checks if the last generated commit is the most up to date one
        c <- getBranchHead $ branch key
        let oid = (getOid . untag . commitOid) c
        s <- liftIO $ withForeignPtr oid oidToSha
        if mode == RunDependenciesSame && Just ver == (readVersion <$> old) && Just s == (generatedCommit <$> old) then
            return $ RunResult ChangedNothing (fromJust oldBin) $ ReadPostR (readPost $ fromJust old)
        else do
            let path = readPath key
            metamap <- metaMap s
            let m = (metamap Map.! path)
            t <- getVersionedFile' (branch key) path
            post <- act m t
            let new = BL.toStrict $ runPut $ put $ ReadPostA ver s post
            return $ RunResult ChangedRecomputeDiff new $ ReadPostR post

defaultReadPost = priority 0 $ readPostr (\t m ->
    return $ readPost' t m
    )
