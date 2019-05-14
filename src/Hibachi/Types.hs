{-# LANGUAGE DeriveGeneric #-}

module Hibachi.Types
    ( Author
    , Post(..)
    , PostCommon(..)
    , FileMetadata(..)
    , PostError

    , toCommon
    , fromParseException
    ) where

import           CMark            (Node)
import           Data.Text        (Text)
import           Data.Time
import           Data.Yaml        (FromJSON, ParseException)
import           GHC.Generics     (Generic)
import           Git              (TreeFilePath)
import           Hibachi.ReadTime

type Author = Text

data Post = PlainPost PostCommon
          | Story
            { prev :: [PostCommon]
            , this :: PostCommon
            , succ :: [PostCommon]
            } deriving (Eq, Show, Ord)

data PostCommon = PostCommon
                { postAuthor     :: Author
                , postKeywords   :: [Text]
                , postTags       :: [Text]
                , postReadTime   :: ReadTime

                , postTitle      :: Node
                , postAbstract   :: Node
                , postContent    :: Node

                , postPostedTime :: ZonedTime
                , postGitPath    :: TreeFilePath
                , postLinkPath   :: FilePath --- The path a href needs to point to
                } deriving (Eq, Show, Ord)

toCommon :: Post -> PostCommon
toCommon (PlainPost c) = c
toCommon (Story _ c _) = c

data FileMetadata = FileMetadata
                  { title    :: Text
                  , abstract :: Text
                  , tags     :: [Text]
                  --, keywords :: [Text]
                  , part     :: Maybe Int
                  } deriving (Eq, Show, Generic)
instance FromJSON FileMetadata

data PostError = YamlErr ParseException
    deriving Show
fromParseException :: ParseException -> PostError
fromParseException = YamlErr

data Project = Project
    { projectTitle       :: Text
    , projectTags        :: Text
    , projectAbstract    :: Node
    , projectDescription :: Node
    , projectImageURL    :: Text
    } deriving (Eq, Show)
