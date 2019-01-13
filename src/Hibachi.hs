module Hibachi where

import Prelude hiding (filter)

import Hibachi.Post
import Hibachi.Read
import Data.Time

import Data.Text.IO as TIO

import Git
import Git.Tree.Working
import Git.Libgit2

import Control.Monad.IO.Class

import Data.Tagged
import Data.Maybe

import Conduit
import Data.Conduit.List hiding (map, mapM_)

libmain = do
    let repoOpts = RepositoryOptions { repoPath = "/home/glr/Documents/Blog/"
                                     , repoWorkingDir = Nothing
                                     , repoIsBare = False
                                     , repoAutoCreate = False
                                     }
    a <- withRepository' lgFactory repoOpts $ do
        refhead <- resolveReference "HEAD"
        tr <- lookupTree . commitTree =<< lookupCommit (Tagged $ fromJust refhead)
        a <- runConduit $ sourceTreeEntries tr .| filter justFiles .| sinkList
        liftIO $ mapM_ printC (snd <$> a)
        return a
    print (map fst a)

  where
    justFiles (_, (BlobEntry _ _)) = True
    justFiles _ = False
    printC (BlobEntry o _) = o
    printC' (Blob _ c) = case c of
        (BlobString bs) -> print bs
        (BlobStringLazy bs) -> print bs
        _ -> return ()
