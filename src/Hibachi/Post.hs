{-# LANGUAGE DeriveGeneric #-}

module Hibachi.Post
    ( Post(..)
    , renderPost
    , generatePost

    , renderIndex

    , renderAbout

    , calculateReadTime
    , ParseException
    )
    where

import GHC.Generics
import GHC.Err

import Prelude hiding (words, unlines, lines)

import Lucid
import Data.Text (Text, intercalate, pack, words, lines, unlines)
import Data.Text.Encoding (encodeUtf8)

import Data.Maybe
import Data.Time
import Data.Yaml

import System.FilePath.Posix

import CMark

import Hibachi.Templates
import Hibachi.ReadTime

data Metadata = Metadata
              { title :: Text
              , abstract :: Text
              , tags :: [Text]
              } deriving (Eq, Show, Generic)
instance FromJSON Metadata

data Post = Post
          { author :: Text
          , keywords :: [Text]

          , preadTime :: ReadTime

          , metadata :: Metadata
          , content :: Node

          , posted :: ZonedTime

          , path :: FilePath
          } deriving (Show)

generatePost :: Text -> ZonedTime -> (FilePath, Text) -> Either ParseException Post
generatePost author time (path, rawcontent) = do
    (m,n) <- parsePostFile rawcontent

    let nx = apply dropHeadingLevel n

    Right $ Post author ["Blog"] (calculateReadTime nx) m nx time path

parsePostFile :: Text -> Either ParseException (Metadata, Node)
parsePostFile c = do
    let (mt, ct) = splitMeta c
        n = commonmarkToNode [optSourcePos, optNormalize, optSmart] ct
    m <- decodeEither' $ encodeUtf8 mt
    Right (m, n)

splitMeta :: Text -> (Text, Text)
splitMeta c = case lines c of
    ("---":xs) -> (unlines $ takeWhile (/= "...") xs, unlines $ tail $ dropWhile (/= "...") xs)
    (x:xs) -> ("", unlines (x:xs))


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

renderPost :: Post -> Html ()
renderPost p = do
    doctype_ 
    html_ [lang_ "en"] $ do
        htmlHead (author p) (abstract$metadata p) (keywords p)
        htmlBody $ do
            article_ [class_ "post"] $ do
                postHeader (title$metadata p) (preadTime p)
                renderContent $ (content p)
                postFooter (posted p) (tags$metadata p) (author p)

renderAbout :: Text -> Html ()
renderAbout about = do
    doctype_
    html_ [lang_ "en"] $ do
        htmlHead "Gregor 'dequbed' Reitzenstein" "" ["Blog", "dequbed"]
        htmlBody $ do
            div_ [class_ "post"]
                $ renderContent
                $ apply dropHeadingLevel
                $ commonmarkToNode [optSmart, optNormalize] about


renderIndex :: [Post] -> Html ()
renderIndex ps = do
    doctype_ 
    html_ [lang_ "en"] $ do
        htmlHead mempty mempty mempty :: Html ()
        htmlBody $ mapM_ renderShortPostlink ps

renderPostlink :: Post -> Html ()
renderPostlink p = a_ [class_ "postlink", href_ (pack $ path p)] $ article_ [class_ "post"] $ do
    postHeader (title$metadata p) (preadTime p)
    toHtmlRaw $ commonmarkToHtml [optSmart, optNormalize] (abstract$metadata p)
    postFooter (posted p) (tags$metadata p) (author p)

renderShortPostlink :: Post -> Html ()
renderShortPostlink p = a_ [class_ "postlink", href_ (pack $ path p)] $ article_ [class_ "post"] $ do
    postShortHeader (title$metadata p)
    toHtmlRaw $ commonmarkToHtml [optSmart, optNormalize] (abstract$metadata p)
    postFooter (posted p) (tags$metadata p) (author p)
