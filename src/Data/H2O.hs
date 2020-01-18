{-# LANGUAGE TemplateHaskell #-}
module Data.H2O 
    where

import Prelude hiding (length)

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

import CMark

import Data.Yaml

import Control.Lens

type UrlT = Text
data Storylinks u = Storylinks 
    { _init :: u 
    , _prev :: u
    , _next :: u
    , _last :: u
    , _about :: u
    } deriving (Eq, Ord, Show, Generic)

makeLenses ''Storylinks

-- | Meta is the part of the post metadata that is extraced from git
data Meta = Meta
    { _author :: Text
    , _email :: Text
    , _posted :: UTCTime
    , _modified :: UTCTime
    } deriving (Eq, Ord, Show, Generic)

instance NFData Meta

instance Binary UTCTime where
    put (UTCTime d dt) = do
        put $ toModifiedJulianDay d
        put $ diffTimeToPicoseconds dt
    get = do
        d <- get
        dt <- get
        return $ UTCTime (ModifiedJulianDay d) (picosecondsToDiffTime dt)
instance Binary Meta

makeLenses ''Meta

data PostHeader = PostHeader
    { _hdrStoryName :: Maybe Text
    , _hdrStoryIdx :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

instance FromJSON PostHeader where
    parseJSON (Object v) = PostHeader
        <$> v .: "story"
        <*> v .: "part"

makeLenses ''PostHeader

data GPost m = GPost
    { _title :: m
    , _abstract :: m
    , _content :: m
    , _meta :: Meta
    , _storyLinks :: Maybe (Storylinks UrlT)
    } deriving (Eq, Ord, Show, Generic)

makeLenses ''GPost

type Post = GPost Node
