{-# LANGUAGE OverloadedStrings #-}
module Data.H2O.Read where

import              Data.ByteString (ByteString)

import              Data.Text (Text)
import qualified    Data.Text as T
import              Data.Text.Encoding (encodeUtf8)

import              Data.Yaml

import              Data.H2O.Types (PostHeader)

readHeader :: Text -> Maybe PostHeader
readHeader = decodeThrow <$> getHeader

getHeader :: Text -> ByteString
getHeader = encodeUtf8 . T.unlines . takeWhile (/= "...") . T.lines
