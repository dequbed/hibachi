module Data.H2O.Shake.Init
    ( hibachiBuild
    ) where

import Development.Shake

import Data.H2O.Shake
import Data.H2O.Shake.Branch
import Data.H2O.Shake.Meta
import Data.H2O.Shake.Post
import Data.H2O.Shake.Index
import Data.H2O.Shake.Tags
import Data.H2O.Shake.Project

-- | Setup function that needs to be called before being able to do any other Actions requiring a
-- repository
hibachiBuild :: FilePath -> Rules () -> IO ()
hibachiBuild repo f = shakeArgs so $ do
    addBranchHeadRule
    addMetaMapRule
    addPostBuildRule
    addPostReadRule
    addIndexBuildRule
    addTagBuildRule
    addProjectReadRule
    addProjectIndexBuildRule
    defaultReadPost
    defaultReadProject
    f
  where
    so = shakeOptions{shakeExtra = addShakeExtra (RepoPath repo) $ shakeExtra shakeOptions}
