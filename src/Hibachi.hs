module Hibachi where

import Hibachi.Post
import Hibachi.Read
import Data.Time

import Data.Text.IO as TIO

import Git
import Git.Libgit2

import Control.Monad.IO.Class


libmain = do
    let repoOpts = RepositoryOptions { repoPath = "/home/glr/Documents/Blog/"
                                     , repoWorkingDir = Nothing
                                     , repoIsBare = False
                                     , repoAutoCreate = False
                                     }
    a <- withRepository' lgFactory repoOpts $ do
        listReferences
    print a
