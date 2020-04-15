module Data.H2O.Post
    ( readPost'
    , readPost
    ) where

import Prelude hiding (lines, unlines)

import CMark
import Data.Yaml

import Data.Either.Combinators (fromRight')

import Data.Text (Text, lines, unlines, pack)
import Data.Text.Encoding (encodeUtf8)

import Data.List.Split

import Data.H2O.Types (Post, Meta, PostHeader, buildPost)
import Data.H2O.ReadTime (calculateReadTime)

type Error = Text

readPost' :: Meta -> Text -> Post
readPost' m t = fromRight' $ readPost m t

readPost :: Meta -> Text -> Either Error Post
readPost meta t = do
    (ht, ct) <- splitT t
    h <- readPHeader ht
    return $ buildPost meta h $ commonmarkToNode [] ct

splitT :: Text -> Either Error (Text, Text)
splitT t = case splitWhen (== "...") $ lines t of
    [h,c] -> Right (unlines h, unlines c)
    _ -> Left "No header YAML found"

readPHeader :: Text -> Either Error PostHeader
readPHeader = mapLeft showT . decodeEither' . encodeUtf8

showT = pack . show
