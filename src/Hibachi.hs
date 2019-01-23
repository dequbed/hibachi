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
import Data.ByteString.Char8 (unpack)

import CMark
import Lucid

libmain = do
    let repoOpts = RepositoryOptions { repoPath = "/home/glr/Documents/Blog/"
                                     , repoWorkingDir = Nothing
                                     , repoIsBare = False
                                     , repoAutoCreate = False
                                     }
    f <- withRepository' lgFactory repoOpts $ do
        -- First off: Check was has happened since the last commit we
        -- generated
        refhead <- resolveReference "HEAD"
        commit <- lookupCommit (Tagged $ fromJust refhead)

        let author = signatureName $ commitAuthor commit
            committime = zonedTimeToUTC $ signatureWhen $ commitCommitter commit
            postB = buildPost author "My Blog" ["Blog"] committime ["TagA"]

        tr <- lookupTree . commitTree $ commit
        a <- runConduit $ sourceTreeEntries tr .| filter justFiles .| sinkList
        let (b, c) = unzip a
        c' <- Prelude.mapM printC c
        return $ (zip b c', postB)
    -- c :: [FilePath, Text] -- [git-path, content]

    let (c, postB) = f
    let (path, c') = unzip c
        path' = Prelude.map unpack path

    let p' = Prelude.map postB c'
        r = Prelude.map (renderText . renderPost) p'

    writeFile "default.css" styleText

    Prelude.mapM_ (uncurry writeFile) $ zip path' r
    return ()

  where
    justFiles (_, (BlobEntry _ _)) = True
    justFiles _ = False
    printC (BlobEntry o _) = catBlobUtf8 o
