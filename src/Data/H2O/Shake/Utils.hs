module Data.H2O.Shake.Utils
    ( writeFileBytes
    , writeFileText
    ) where

import qualified Prelude.FilePath as Path
import qualified Prelude.Directory as Directory 

import qualified Data.ByteString as BS

import Development.Shake (Action)

-- Write a file, taking an prefix from ShakeExtra (usually "out")
writeFileText :: FilePath -> Text -> Action ()
writeFileText path content = do
    Directory.createDirectoryIfMissing True (Path.takeDirectory path)
    Prelude.writeFileUtf8 path content

writeFileBytes :: FilePath -> BS.ByteString -> Action ()
writeFileBytes path content = do
    Directory.createDirectoryIfMissing True (Path.takeDirectory path)
    liftIO $ BS.writeFile path content
