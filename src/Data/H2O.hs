{-# LANGUAGE TemplateHaskell #-}
module Data.H2O
    ( buildPost
    , Post(..)
    , title
    , abstract
    , content
    , readtime
    , tags
    , author
    , email
    , posted
    , modified
    , story

    , Story(..)
    , storyName
    , storyPart

    , Meta(..)
    , metaAuthor
    , metaEmail
    , metaPosted
    , metaModified

    , PostHeader(..)
    , hdrStoryName
    , hdrStoryIdx
    , hdrTitle
    , hdrTags
    , hdrAbstract
    ) where

import Development.Shake.Classes
import GHC.Generics (Generic)
import Data.Binary (Binary, put, get)
import Data.Binary.Put
import Data.Binary.Get
import Data.Text (Text)
import Data.Time (UTCTime(..), Day(..), DiffTime)
import Data.Time.Clock (diffTimeToPicoseconds, picosecondsToDiffTime)
import Lucid (Html)
import Data.ByteString.Lazy (length, fromStrict, toStrict)
import Data.Maybe
import Control.Monad.IO.Class
import Data.Map (Map)
import Git (TreeFilePath)

import CMark

import Data.Yaml

import Control.Lens (makeLenses)

import Data.H2O.ReadTime (ReadTime, calculateReadTime)

-- | Meta is the part of the post metadata that is extraced from git
data Meta = Meta
    { _metaAuthor :: Text
    , _metaEmail :: Text
    , _metaPosted :: UTCTime
    , _metaModified :: UTCTime
    } deriving (Eq, Ord, Show, Generic)
makeLenses ''Meta

instance NFData Meta

instance Binary UTCTime where
    put (UTCTime d dt) = do
        put $ toModifiedJulianDay d
        put $ diffTimeToPicoseconds dt
    get = do
        d <- ModifiedJulianDay <$> get
        dt <- picosecondsToDiffTime <$> get
        return $ UTCTime d dt

instance Binary Meta


-- | Each post has a YAML header that contains information about the post
data PostHeader = PostHeader
    { _hdrStoryName :: Maybe Text
    , _hdrStoryIdx :: Maybe Int
    , _hdrTitle :: Node
    , _hdrTags :: [Text]
    , _hdrAbstract :: Node
    } deriving (Eq, Ord, Show, Generic)
makeLenses ''PostHeader

instance FromJSON PostHeader where
    parseJSON (Object v) = PostHeader
        <$> v .:? "story"
        <*> v .:? "part"
        <*> fmap (commonmarkToNode []) (v .: "title")
        <*> v .:? "tags" .!= []
        <*> fmap (commonmarkToNode []) (v .: "abstract")

-- | A Story is generally just a list of its parts, identified by a shared story name.
data Story = Story
    { _storyName :: Text
    , _storyPart :: Int
    } deriving (Eq, Ord, Show, Generic)
makeLenses ''Story

-- | Post is the central data structure of Hibachi
data Post = Post
    { _title :: Node -- ^ The title which is displayed in the list and detail views
    , _abstract :: Node -- ^ A short abstract to be displayed in the list view
    , _content :: Node -- ^ The actual content of the post, displayed in the detail view
    , _readtime :: ReadTime -- ^ Calucated reading time, displayed in list and detail view
    , _tags :: [Text] -- ^ Tags, used for sorting in list view. Displayed in list and detail view
    , _author :: Text -- ^ Author of the blog post as read from git commit
    , _email :: Text -- ^ Email of the author, as read from the git commit
    , _posted :: UTCTime -- ^ Time this post was first created, read from git commit
    , _modified :: UTCTime -- ^ Time this post was last modified, read from git commit
    , _story :: Maybe Story -- ^ If the post is part of a story, keep that data here
    } deriving (Eq, Ord, Show, Generic)
makeLenses ''Post

-- | Build a Blog post from its parts
buildPost :: Meta -> PostHeader -> Node -> Post
buildPost m h content = Post 
    (h^.hdrTitle) 
    (h^.hdrAbstract) 
    content 
    (calculateReadTime content)
    (h^.hdrTags)
    (m^.metaAuthor)
    (m^.metaEmail)
    (m^.metaPosted)
    (m^.metaModified)
    (Story <$> (h^.hdrStoryName) <*> (h^.hdrStoryIdx))
