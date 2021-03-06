{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
module Data.H2O.Types
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
    , indexshow

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
    , hdrIndex

    , buildProject
    , Project(..)
    , projName
    , projIdx
    , projPicture
    , projDesc
    , projLink

    , ProjectHeader(..)
    , hdrProjName
    , hdrProjIdx
    , hdrProjPicture
    , hdrProjLink
    ) where

import qualified Prelude.ByteString.Lazy as BL

import System.IO
import System.IO.Unsafe

import Development.Shake.Classes
import GHC.Generics (Generic)
import Data.Binary (Binary, put, get, encode, decode)
import Data.Binary.Put
import Data.Binary.Get
import Data.Text (Text, pack)
import Data.Time (UTCTime(..), Day(..), DiffTime)
import Data.Time.Clock (diffTimeToPicoseconds, picosecondsToDiffTime)
import Lucid (Html)
import Data.ByteString.Lazy (length, fromStrict, toStrict)
import Data.Maybe
import Control.Monad.IO.Class
import Data.Map (Map)
import Git (TreeFilePath)

import Text.Read (read)

import CMark

import Data.Yaml hiding (encode, decode)

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
    -- ^ Name of the story this post is part of — if any
    , _hdrStoryIdx :: Maybe Int
    -- ^ If part of a story, what part is this?
    , _hdrTitle :: Node
    -- ^ Title of the post. A Node because it can be full commonmark
    , _hdrTags :: [Text]
    -- ^ Tags to file this post under. It's the job of the library consumer to
    -- actually provide tag-based sorting
    , _hdrAbstract :: Node
    -- ^ A short abstract. Can be shown in a posting overview
    , _hdrIndex :: Bool
    -- ^ Put this post on the index or nah? Defaults to Yes.
    } deriving (Eq, Ord, Show, Generic)
makeLenses ''PostHeader

instance FromJSON PostHeader where
    parseJSON (Object v) = PostHeader
        <$> v .:? "story"
        <*> v .:? "part"
        <*> fmap (commonmarkToNode []) (v .: "title")
        <*> v .:? "tags" .!= []
        <*> fmap (commonmarkToNode []) (v .: "abstract")
        <*> v .:? "index" .!= True

-- | A Story is generally just a list of its parts, identified by a shared story name.
data Story = Story
    { _storyName :: Text
    , _storyPart :: Int
    } deriving (Eq, Ord, Show, Generic, Hashable)
makeLenses ''Story

instance Binary Story
instance NFData Story

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
    , _indexshow :: Bool
    } deriving (Eq, Ord, Show, Generic)
makeLenses ''Post

instance Binary PosInfo
instance Binary NodeType
instance Binary ListAttributes
instance Binary ListType
instance Binary DelimType
instance Binary Node
instance Binary Post

instance NFData PosInfo
instance NFData NodeType
instance NFData ListAttributes
instance NFData ListType
instance NFData DelimType
instance NFData Node
instance NFData Post

instance Hashable Post where
    hashWithSalt :: Int -> Post -> Int
    hashWithSalt s (Post title abstract content readtime tags author email posted modified story index) = 
        s `hashWithSalt`
        show title `hashWithSalt`
        show abstract `hashWithSalt`
        show content `hashWithSalt`
        readtime `hashWithSalt`
        tags `hashWithSalt`
        author `hashWithSalt`
        email `hashWithSalt`
        show posted `hashWithSalt`
        show modified `hashWithSalt`
        story `hashWithSalt`
        index

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
    (h^.hdrIndex)

data ProjectHeader = ProjectHeader
    { _hdrProjName :: Node
    , _hdrProjIdx :: Int
    , _hdrProjPicture :: Text
    , _hdrProjLink :: Text
    } deriving (Eq, Ord, Show, Generic)
makeLenses ''ProjectHeader

instance FromJSON ProjectHeader where
    parseJSON (Object v) = ProjectHeader
        <$> fmap (commonmarkToNode []) (v .: "name")
        <*> v .: "index"
        <*> v .: "picture"
        <*> v .: "link"

data Project = Project
    { _projName :: Node
    , _projIdx :: Int
    , _projPicture :: Text
    , _projDesc :: Node
    , _projLink :: Text
    } deriving (Eq, Ord, Show, Generic)
makeLenses ''Project

buildProject :: ProjectHeader -> Node -> Project
buildProject (ProjectHeader name idx pic link) desc =
    Project name idx pic desc link

instance Binary Project
instance NFData Project

instance Hashable Project where
    hashWithSalt :: Int -> Project -> Int
    hashWithSalt s (Project name order picture desc link) =
        s `hashWithSalt`
        show name `hashWithSalt`
        order `hashWithSalt`
        picture `hashWithSalt`
        show desc `hashWithSalt`
        link
