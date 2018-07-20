module Hibachi.Post
    ( Post(..)
    , buildPost
    , extractBody
    , extractTitle
    , extractAbstract
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

import Data.Time

import Text.Pandoc

import Hibachi.Types

data Post = Post
    { body :: [Block]
    , title :: [Inline]
    , abstract :: [Block]
    , tags :: [Tag]
    , authors :: [Author]
    , posted :: UTCTime
    , edited :: UTCTime
    } deriving (Eq, Show)

buildPost :: Pandoc -> [Author] -> UTCTime -> UTCTime -> Result Post
buildPost (Pandoc m c) as p e = do
        t <- buildTitle =<< metalookup m "title"
        a <- buildAbstract =<< metalookup m "abstract"
        ts <- buildTagsM $ metalookupM m "tags"
        return (Post c t a ts as p e)
    where
        metalookup m t = maybe (Left $ MetaLookupFailed $ T.pack t) pure $ metalookupM m t
        metalookupM m = flip M.lookup (unMeta m)

buildTitle :: MetaValue -> Result [Inline]
buildTitle (MetaInlines i) = Right i
buildTitle (MetaString s) = Right [Str s]
buildTitle _ = Left InvalidTitleType

buildAbstract :: MetaValue -> Result [Block]
buildAbstract (MetaBlocks b) = Right b
buildAbstract (MetaInlines i) = Right [Para i]
buildAbstract (MetaString s) = Right [Para [Str s]]
buildAbstract _ = Left InvalidAbstractType

buildTagsM (Just v) = buildTags v
buildTagsM Nothing = Right []

buildTags :: MetaValue -> Result [Tag]
buildTags (MetaList xs) = sequence $ fmap buildTag xs
buildTags _ = Left InvalidTagsType

buildTag :: MetaValue -> Result Tag
buildTag (MetaString s) = Right $ T.pack s
buildTag _ = Left InvalidTagType

extractBody :: Post -> Pandoc
extractBody p = Pandoc nullMeta $ body p

extractTitle :: Post -> Pandoc
extractTitle p = Pandoc nullMeta $ [Plain $ title p]

extractAbstract :: Post -> Pandoc
extractAbstract p = Pandoc nullMeta $ abstract p
