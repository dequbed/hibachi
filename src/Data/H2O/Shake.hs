module Data.H2O.Shake 
    ( getRepo
    , RepoPath(..)
    )
    where

import Development.Shake

-- | Shake option for the path of the repository
newtype RepoPath = RepoPath FilePath
getRepo :: Action FilePath
getRepo = getShakeExtra >>= \case
    Just (RepoPath r) -> return r
    Nothing -> fail "Repository path isn't defined"
