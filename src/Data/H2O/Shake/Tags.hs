{-# LANGUAGE DeriveAnyClass #-}
module Data.H2O.Shake.Tags
    ( writeTags
    , wantTagIndex
    , addTagBuildRule
    ) where

import qualified Prelude.Map as Map
import qualified Prelude.List as L
import qualified Prelude.ByteString.Lazy as BL

import Data.H2O.Types
import Data.H2O.Shake
import Data.H2O.Shake.Post
import Data.H2O.Shake.Branch
import Data.H2O.Shake.Index (needBranchPosts)

import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Data.Bifunctor (first)

import Data.ByteString.UTF8 (toString)

import Data.Binary.Put
import Data.Binary.Get

import Data.Maybe (fromJust)

import Development.Shake
import Development.Shake.Rule
import Development.Shake.Classes

import Git
import Git.Libgit2 (lgFactory)
import Git.Types (TreeEntry(..))

newtype TagsRule = TagsRule (Text -> [(FilePath, Post)] -> Action ())
    deriving Typeable

type instance RuleResult TagsQ = TagsR

data TagsQ = TagsQ { fromTagsQ :: [(FilePath, Post)], tagQ :: Text }
    deriving (Typeable, Eq, Show, Generic, Hashable)

instance Binary TagsQ
instance NFData TagsQ

data TagsA = TagsA { version :: Int, fromTagsA :: [(FilePath, Post)], tagA :: Text }
    deriving (Typeable, Generic)

instance Binary TagsA

data TagsR = TagsR
    deriving (Typeable, Show, Generic)

instance NFData TagsR

writeTags :: (Text -> [(FilePath, Post)] -> Action ()) -> Rules ()
writeTags act = addUserRule $ TagsRule act

needTags :: [(FilePath, Post)] -> Text -> Action TagsR
needTags posts tag = apply1 $ TagsQ posts tag

wantTagIndex :: Text -> Rules ()
wantTagIndex branch = action $ do
    l <- needBranchPosts branch
    let m = toMap l
    mapM_ needT $ Map.toList m
  where
    toMap :: [(FilePath, Post)] -> Map Text [(FilePath, Post)]
    toMap list = Map.fromListWith (++) $ L.concat $ map (\e@(_, Post _ _ _ _ tags _ _ _ _ _) -> zip tags $ L.repeat [e]) list
    needT (tag, posts) = needTags posts tag

addTagBuildRule :: Rules ()
addTagBuildRule = addBuiltinRule noLint noIdentity run
  where
    run :: BuiltinRun TagsQ TagsR
    run key oldBin mode = do
        let old = fmap (runGet get . BL.fromStrict) oldBin
            disp _ = Just "tags build rule"
            test (TagsRule b) = Just b

        (ver, act) <- getUserRuleOne key disp test
        let posts = fromTagsQ key
            tags = tagQ key

        if mode == RunDependenciesSame && Just ver == (version <$> old) && Just posts == (fromTagsA <$> old) then
            return $ RunResult ChangedNothing (fromJust oldBin) TagsR
        else do
            act tags posts
            let new = BL.toStrict $ runPut $ put $ TagsA ver posts tags
            return $ RunResult ChangedNothing new TagsR
