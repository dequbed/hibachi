module Data.H2O.Project
    ( readProject'
    , readProject
    ) where

import Prelude hiding (lines, unlines)

import CMark
import Data.Yaml

import Data.Either.Combinators (fromRight')

import Data.Text (Text, lines, unlines, pack)
import Data.Text.Encoding (encodeUtf8)

import Data.List.Split

import Data.H2O.Types (Project, buildProject, ProjectHeader)
import Data.H2O.ReadTime (calculateReadTime)

type Error = Text

readProject' :: Text -> Project
readProject' t = fromRight' $ readProject t

readProject :: Text -> Either Error Project
readProject t = do
    (ht, ct) <- splitT t
    h <- readPHeader ht
    return $ buildProject h $ commonmarkToNode [] ct

splitT :: Text -> Either Error (Text, Text)
splitT t = case splitWhen (== "...") $ lines t of
    [h,c] -> Right (unlines h, unlines c)
    _ -> Left "No header YAML found"

readPHeader :: Text -> Either Error ProjectHeader
readPHeader = mapLeft showT . decodeEither' . encodeUtf8

showT = pack . show
