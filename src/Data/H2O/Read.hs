module Data.H2O.Read where

import qualified    Data.ByteString.Char8 as BS
import              Data.ByteString (ByteString)
import              Data.Yaml

import              Data.H2O

readHeader :: ByteString -> Maybe PostHeader
readHeader = decode
