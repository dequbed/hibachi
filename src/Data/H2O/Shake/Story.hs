{-# LANGUAGE DeriveAnyClass #-}
module Data.H2O.Shake.Story
    ( writeStories
    , genStoryIndex
    , addStoryBuildRule
    ) where

import qualified Prelude.Map as Map
import qualified Prelude.List as L
import qualified Prelude.ByteString.Lazy as BL

import Data.H2O.Types
import Data.H2O.Shake
import Data.H2O.Shake.Post
import Data.H2O.Shake.Branch

import Data.Conduit
import qualified Data.Conduit.Combinators as C

import Data.Binary.Put
import Data.Binary.Get

import Data.Maybe (fromJust)

import Development.Shake
import Development.Shake.Rule
import Development.Shake.Classes

import Git
import Git.Libgit2 (lgFactory)
import Git.Types (TreeEntry(..))

newtype StoriesRule = StoriesRule (Text -> [(FilePath, Post)] -> Action ())
    deriving Typeable

type instance RuleResult StoriesQ = StoriesR

data StoriesQ = StoriesQ { fromStoriesQ :: [(FilePath, Post)], tagQ :: Text }
    deriving (Typeable, Eq, Show, Generic, Hashable)

instance Binary StoriesQ
instance NFData StoriesQ

data StoriesA = StoriesA { version :: Int, fromStoriesA :: [(FilePath, Post)], tagA :: Text }
    deriving (Typeable, Generic)

instance Binary StoriesA

data StoriesR = StoriesR
    deriving (Typeable, Show, Generic)

instance NFData StoriesR

writeStories :: (Text -> [(FilePath, Post)] -> Action ()) -> Rules ()
writeStories act = addUserRule $ StoriesRule act

needStories :: [(FilePath, Post)] -> Text -> Action StoriesR
needStories posts story = apply1 $ StoriesQ posts story

genStoryIndex :: Text -> Rules ()
genStoryIndex branch = action $ do
    tree <- branchTree branch
    repo <- getRepo
    b <- liftIO $ withRepository lgFactory repo $
        runConduit $ sourceTreeEntries tree 
            .| C.filter (\case
                (_, BlobEntry _ _) -> True
                _ -> False)
            .| C.sinkList
    posts <- needPosts branch $ map fst b
    paths <- writePosts posts

    let m = toMap $ zip paths posts
    mapM_ needT $ Map.toList m
  where
    toMap :: [(FilePath, Post)] -> Map Text [(FilePath, Post)]
    toMap list = Map.fromListWith (++) $ L.concat $ map (\e@(_, Post _ _ _ _ _ _ _ _ _ ms) -> case ms of
            Just story -> [(_storyName story, [e])]
            Nothing -> []
        ) list
    needT (tag, posts) = needStories posts tag

addStoryBuildRule :: Rules ()
addStoryBuildRule = addBuiltinRule noLint noIdentity run
  where
    run :: BuiltinRun StoriesQ StoriesR
    run key oldBin mode = do
        let old = fmap (runGet get . BL.fromStrict) oldBin
            disp _ = Just "tags build rule"
            test (StoriesRule b) = Just b

        (ver, act) <- getUserRuleOne key disp test
        let posts = fromStoriesQ key
            tags = tagQ key

        if mode == RunDependenciesSame && Just ver == (version <$> old) && Just posts == (fromStoriesA <$> old) then
            return $ RunResult ChangedNothing (fromJust oldBin) StoriesR
        else do
            act tags posts
            let new = BL.toStrict $ runPut $ put $ StoriesA ver posts tags
            return $ RunResult ChangedNothing new StoriesR
