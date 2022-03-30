module Data.H2O.Shake.Init
    ( hibachiBuild
    ) where

import System.Console.GetOpt

import Development.Shake

import Data.H2O.Shake
import Data.H2O.Shake.Branch
import Data.H2O.Shake.Meta
import Data.H2O.Shake.Post
import Data.H2O.Shake.Index
import Data.H2O.Shake.Tags

newtype Flag = FlagPosts FilePath

flags = [Option "" ["posts"] (ReqArg (Right . FlagPosts) "DIR") "Posts Repository path"]

-- | Setup function that needs to be called before being able to do any other Actions requiring a
-- repository
hibachiBuild :: Rules () -> IO ()
hibachiBuild f = shakeArgsOptionsWith shakeOptions flags $ \opts flags targets -> do
    pure $ Just ( setOpts opts flags , do
            addBranchHeadRule
            addMetaMapRule
            addPostBuildRule
            addPostReadRule
            addIndexBuildRule
            addTagBuildRule
            defaultReadPost
            f
        )

setOpts :: ShakeOptions -> [Flag] -> ShakeOptions
setOpts = foldr setOpt

setOpt :: Flag -> ShakeOptions -> ShakeOptions
setOpt (FlagPosts repo) opts = opts{shakeExtra = addShakeExtra (RepoPath repo) $ shakeExtra opts}
