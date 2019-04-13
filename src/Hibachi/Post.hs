{-# LANGUAGE DeriveGeneric #-}

module Hibachi.Post
    ( Post(..)
    , PostCommon(..)
    , PostError
    , generatePost
    )
    where

import GHC.Generics
import GHC.Err

import Prelude hiding (words, unlines, lines)

import Lucid
import Data.Text (Text, intercalate, pack, words, lines, unlines)
import Data.Text.Encoding (encodeUtf8)

import Data.Maybe
import Data.Either.Combinators
import Data.Time
import Data.Yaml

import qualified Data.ByteString.Char8 as BS

import System.FilePath.Posix

import CMark

import Hibachi.ReadTime

import Git (TreeFilePath)

type Author = Text

data Post = PlainPost PostCommon
          | Story
            { prev :: [PostCommon]
            , this :: PostCommon
            , succ :: [PostCommon]
            }

data PostCommon = PostCommon
                { postAuthor :: Author
                , postKeywords :: [Text]
                , postTags :: [Text]
                , postReadTime :: ReadTime

                , postTitle :: Node
                , postAbstract :: Node
                , postContent :: Node

                , postPostedTime :: ZonedTime
                , postGitPath :: TreeFilePath
                , postLinkPath :: FilePath --- The path a href needs to point to
                }

data FileMetadata = FileMetadata
                  { title :: Text
                  , abstract :: Text
                  , tags :: [Text]
                  , keywords :: [Text]
                  , part :: Maybe Int
                  } deriving (Eq, Show, Generic)
instance FromJSON FileMetadata

data PostError = YamlErr ParseException
    deriving Show
fromParseException :: ParseException -> PostError
fromParseException = YamlErr

generatePost :: Author -> ZonedTime -> TreeFilePath -> Text -> Either PostError Post
generatePost author postedTime path filecontent =
    PlainPost <$> generateCommon author postedTime path filecontent

generateStory :: Author -> ZonedTime -> TreeFilePath -> [PostCommon] -> [PostCommon] -> Text -> Either PostError Post
generateStory author postedTime path prev next filecontent = do
    common <- generateCommon author postedTime path filecontent
    return $ Story prev common next

generateCommon :: Author -> ZonedTime -> TreeFilePath -> Text -> Either PostError PostCommon
generateCommon author postedTime path filecontent = do
    (m, content) <- mapLeft fromParseException $ parsePostFile filecontent
    let content' = apply dropHeadingLevel content
    Right $ PostCommon
        author
        (keywords m)
        (tags m)
        (calculateReadTime content')
        (commonmarkToNode [optSmart] $ title m)
        (commonmarkToNode [optSmart] $ abstract m)
        content'
        postedTime
        path
        (toLinkPath path)

toLinkPath = BS.unpack

parsePostFile :: Text -> Either ParseException (FileMetadata, Node)
parsePostFile c = do
    let (mt, ct) = splitMeta c
        n = commonmarkToNode [optSourcePos, optNormalize, optSmart] ct
    m <- decodeEither' $ encodeUtf8 mt
    Right (m, n)

splitMeta :: Text -> (Text, Text)
splitMeta c = case lines c of
    ("---":xs) -> (unlines $ takeWhile (/= "...") xs, unlines $ tail $ dropWhile (/= "...") xs)
    (x:xs) -> ("", unlines (x:xs))

apply :: (Node -> Node) -> Node -> Node
apply f n = let (Node p t ns) = f n in
    Node p t $ map (apply f) ns

calculateReadTime :: Node -> ReadTime
calculateReadTime (Node _ nt ns) = foldr (addTime . calculateReadTime) (calculateReadTime' nt) ns

calculateReadTime' :: NodeType -> ReadTime
calculateReadTime' (HTML_BLOCK t)   = ReadTime (length $ words t) 0
calculateReadTime' (HTML_INLINE t)  = ReadTime (length $ words t) 0
calculateReadTime' (CODE_BLOCK _ t) = ReadTime (length $ words t) 0
calculateReadTime' (CODE t)         = ReadTime (length $ words t) 0
calculateReadTime' (TEXT t)         = ReadTime (length $ words t) 0
calculateReadTime' (LINK _ t)       = ReadTime (length $ words t) 0
calculateReadTime' (IMAGE _ t)      = ReadTime (length $ words t) 1
calculateReadTime' _                = ReadTime 0 0


dropHeadingLevel' :: NodeType -> NodeType
dropHeadingLevel' (HEADING 6) = STRONG
dropHeadingLevel' (HEADING x) = HEADING (x+1)
dropHeadingLevel' nt          = nt

dropHeadingLevel :: Node -> Node
dropHeadingLevel (Node p t ns) = Node p (dropHeadingLevel' t) ns
