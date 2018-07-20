module Hibachi.Read
    ( readPost
    ) where

import Hibachi.Types
import Hibachi.Post

import Data.Text
import Data.Time

import Text.Pandoc

readPost :: Text -> [Author] -> UTCTime -> UTCTime -> IO (Result Post)
readPost t as p e = do
    d <- readMD t
    return $ buildPost d as p e

readMD :: Text -> IO Pandoc
readMD t = runIOorExplode $ readMarkdown mdOpts t

mdOpts = def
    { readerStandalone = True
    , readerExtensions = mdExts
    }

mdExts = pandocExtensions
