module Hibachi where

import Prelude hiding (filter, writeFile)

import Hibachi.Post
import Hibachi.Style

import Data.Time

import Git
import Git.Tree.Working
import Git.Libgit2

import Control.Monad.IO.Class

import Data.Tagged
import Data.Maybe

import Conduit
import Data.Conduit.List hiding (mapM_)

import Data.Text.Lazy.IO

import CMark
import Lucid

libmain = do
    let repoOpts = RepositoryOptions { repoPath = "/home/glr/Documents/Blog/"
                                     , repoWorkingDir = Nothing
                                     , repoIsBare = False
                                     , repoAutoCreate = False
                                     }
    c <- withRepository' lgFactory repoOpts $ do
        -- First off: Check was has happened since the last commit we
        -- generated
        refhead <- resolveReference "HEAD"
        tr <- lookupTree . commitTree =<< lookupCommit (Tagged $ fromJust refhead)
        a <- runConduit $ sourceTreeEntries tr .| filter justFiles .| sinkList
        let (b, c) = unzip a
        c' <- Prelude.mapM printC c
        return $ zip b c'
    -- c :: [FilePath, Text] -- [git-path, content]

    let (p, c') = unzip c
        h = Prelude.map (commonmarkToHtml [optSmart, optNormalize]) c'
        l = Prelude.map wrapPost h
        l' = Prelude.map renderText l
        p' = Prelude.map show p

    putCss ourStyle

    Prelude.mapM_ (uncurry writeFile) $ zip p' l'

  where
    justFiles (_, (BlobEntry _ _)) = True
    justFiles _ = False
    printC (BlobEntry o _) = catBlobUtf8 o
