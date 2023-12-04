module Data.H2O.Shake.LFS 
    ( getGitLfsBlob
    ) where

import System.IO (print)

import Prelude.FilePath ((</>))
import Prelude.Directory (doesDirectoryExist)
import qualified Prelude.Text as T
import qualified Prelude.ByteString as B

import Development.Shake
import Development.Shake.Rule
import Development.Shake.Classes

import Data.H2O.Shake
import Data.H2O.Shake.Branch (BranchName, getVersionedFile)

import Git hiding (writeIndex)
import Git.Types (TreeEntry(..), sourceTreeEntries)
import Git.Libgit2 (lgFactory, repoOptions)
import Git.Libgit2.Types (lgRepoPath)

data LfsPointer = LfsPointer
    { lfsPointerVersion :: T.Text
    , lfsPointerOid :: T.Text
    , lfsPointerSize :: Integer
    }

tryParseLfsPointer :: T.Text -> Either String LfsPointer
tryParseLfsPointer inp = tryParseLfsPointerLines $ T.lines inp
  where
    tryParseLfsPointerLines :: [T.Text] -> Either String LfsPointer
    tryParseLfsPointerLines [versionLine, oidLine, sizeLine] = do
        version <- maymap "Not a LFS pointer: version line malformed"
            $ T.stripPrefix "version " versionLine

        oid <- maymap "Not a LFS pointer: oid line malformed"
            $ T.stripPrefix "oid sha256:" oidLine

        size <- maymap "Not a LFS pointer: size line malformed"
            $ T.stripPrefix "size " sizeLine >>= readMaybe . T.unpack

        return $ LfsPointer version oid size
      where 
          maymap e = maybe (Left e) Right

    tryParseLfsPointerLines _ = Left "Not a LFS pointer: Bad number of lines"

lfsPointerBlobPath :: FilePath -> LfsPointer -> FilePath
lfsPointerBlobPath prefix (LfsPointer _ oid _) = do
    let letters_1 = take 2 $ T.unpack oid
    let letters_2 = take 2 $ drop 2 $ T.unpack oid
    prefix </> "lfs" </> "objects" </> letters_1 </> letters_2 </> T.unpack oid

getGitLfsBlob :: BranchName -> FilePath -> Action B.ByteString
getGitLfsBlob branch tree_path = do
    contents <- getVersionedFile branch tree_path
    case tryParseLfsPointer contents of
        Left err -> error err
        Right lfsPtr -> do
            repo <- getRepo
            test <- liftIO $ Prelude.Directory.doesDirectoryExist $ repo </> ".git"
            let blobPath = lfsPointerBlobPath (to_repo_path test repo) lfsPtr
            liftIO $ B.readFile blobPath
  where
    to_repo_path :: Bool -> FilePath -> FilePath
    to_repo_path False path = path
    to_repo_path True path = path </> ".git"
