module Data.H2O.Post
    where

import Data.Either.Combinators

import Data.Text (Text)

import Data.H2O

type Error = Text

readPost' :: Text -> Post
readPost' = fromRight' . readPost

readPost :: Text -> Either Error Post
readPost t = readMeta t >>= readContent

readMeta :: Text -> Either Error (Meta, Text)
readMeta = undefined

readContent :: (Meta, Text) -> Either Error Post
readContent = undefined
