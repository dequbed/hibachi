module Data.H2O.Filters
    ( filterLfs
    ) where

import System.IO (print)

import Prelude.Directory (createDirectoryIfMissing)
import Prelude.FilePath (takeDirectory, dropDrive, (</>))
import Prelude.Text (unpack, pack)

import Data.ByteString (writeFile)

import Development.Shake

import Data.H2O.Shake.LFS
import Data.H2O.Shake.Utils (writeFileBytes)

import Network.URI

import CMark

apply :: (Node -> Action Node) -> Node -> Action Node
apply f n = do
    (Node p t ns) <- f n
    ns <- mapM (apply f) ns
    pure (Node p t ns)

wrap :: (NodeType -> Action NodeType) -> Node -> Action Node
wrap f (Node p t ns) = do 
    n <- f t
    pure (Node p n ns)

filterLfs :: Node -> Action Node
filterLfs = apply (wrap filterLfs')
  where
    filterLfs' :: NodeType -> Action NodeType
    filterLfs' (IMAGE url title) 
      | Just uri <- parseURI (unpack url) = do
          uri <- resolveLfsLinks uri
          return (IMAGE (pack $ uriToString id uri "") title)
    filterLfs' nt = pure nt

resolveLfsLinks :: URI -> Action URI
resolveLfsLinks (URI "lfs:" _ p _ _) = do
    -- make sure the path if relative: /abs/file.png -> abs/file.png
    let p2 = dropDrive p
    -- TODO: make this a rule so it's cached
    blob <- getGitLfsBlob "images" p2
    -- Prefix the path with an 'i': abs/file.png -> i/abs/file.png
    let p3 = "i" </> p2
    writeFileBytes p3 blob
    -- The URI we return needs to be absolute: i/abs/file.png -> /i/abs/file.png
    let p4 = "/" </> p3
    pure (URI "" Nothing p4 "" "")
resolveLfsLinks uri = pure uri
