module Data.H2O.Shake.Post
    ( getPosts
    , getStories
    ) where

import qualified Prelude.List as L
import qualified Prelude.Map as Map

import Development.Shake

import Data.H2O.Types (Post(..), story, storyName)
import Data.H2O.Shake.Branch

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

