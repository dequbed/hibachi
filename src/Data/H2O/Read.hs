{-# LANGUAGE OverloadedStrings #-}
module Data.H2O.Read where

import qualified    Data.ByteString.Char8 as BS
import              Data.ByteString (ByteString)

import              Data.Text (Text)
import qualified    Data.Text as T
import              Data.Text.Encoding (encodeUtf8)

import              Data.Yaml

import              Data.H2O

readHeader :: Text -> Maybe PostHeader
readHeader = decodeThrow <$> getHeader

getHeader :: Text -> ByteString
getHeader = encodeUtf8 . T.unlines . takeWhile (/= "...") . T.lines
