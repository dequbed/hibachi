module Data.H2O.Shake.Post
    ( writePost
    , getPosts
    , getStories
    ) where

import qualified Prelude.List as L
import qualified Prelude.Map as Map

import Development.Shake
import Development.Shake.Rule

import Data.H2O.Types (Post(..), story, storyName)
import Data.H2O.Shake.Branch

import Git.Types (SHA)

newtype PostRule = PostRule (Post -> Action ())
    deriving Typeable

type instance RuleResult PostQ = PostR

data PostQ = PostQ { postOid :: SHA, fromPostQ :: Post }
    deriving Typeable

data PostA = PostA { blobOid :: SHA, version :: Int, path :: FilePath }
    deriving Typeable

newtype PostR = PostR FilePath
    deriving Typeable

-- | Define a rule how to write a post.
--   H2O knows *what* posts do build but not /how/. This is the way for an user to tell it.
writePost :: (Post -> Action ()) -> Rules ()
writePost act = addUserRule $ PostRule act

-- | Get a List of all Posts in the index of the given branch, sorted newest to oldest
getPosts :: BranchName -> Action [Post]
getPosts = undefined

-- | Get a Map of all storied posts in the index of the given branch
getStories :: BranchName -> Action (Map Text [Post])
getStories b = do
    posts <- getPosts b
    return $ L.foldl (\m post -> case post^.story of
        Just s -> Map.insertWith (<>) (s^.storyName) [post] m
        Nothing -> m) Map.empty posts

addPostBuildRule :: Rules ()
addPostBuildRule = addBuiltinRule noLint noIdentity run
  where
    run :: BuiltinRun PostQ PostR
    run key oldBin mode = do
        let old = fmap getEx oldBin
            disp _ = Just "This is a test string because I'm not sure what to put here. It's called `disp`."
            test (PostRule b) = Just b
        -- Get the user-defined `writePost` rule
        (ver, act) <- getUserRuleOne key disp test
        -- Use that rule to actually write the post
        let a = act key
        if mode == RunDependenciesSame && Just ver == (version <$> old) && postOid key == (blobOid <$> old) then
            return $ RunResult ChangedNothing old $ PostR (path old)
        else do
            new <- a
            return $ RunResult ChangedRecomputeDiff new $ PostR (path new)

-- Also, make user rules something to get from a TreeFilePath/Blob/Text to a Post? Res. patterns
-- over the kind of file so that users can define how .md and .tex are read into a post. (w/ low
-- `priority` default rules?)
--
-- Two rules: WritePost that writes a Post, returns the path it was written to (important for url
-- resolution). ReadPost that takes a TreeFilePath and generates a Post from that (so that users can
-- use TeX or Markdown or RST instead of commonmark).
