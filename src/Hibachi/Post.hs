{-# LANGUAGE DeriveGeneric #-}

module Hibachi.Post
    ( Post(..)
    , renderPost
    , generatePost

    , renderIndex

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

import Data.Time
import Data.Time.Clock
import Data.Time.Format

import Data.Maybe

import Data.Yaml

import System.FilePath.Posix

import CMark

import Debug.Trace

instance Eq ZonedTime where
    a == b = (zonedTimeToUTC a) == (zonedTimeToUTC b)
instance Ord ZonedTime where
    compare a b = compare (zonedTimeToUTC a) (zonedTimeToUTC b)

data ReadTime = ReadTime
              { numWords :: Int
              , numImages :: Int
              }
    deriving (Eq, Ord)
instance Show ReadTime where
    show r@(ReadTime w i) =
        let m = minutesReadTime r in
            if m <= 1 -- Our algorithm would read "0/1 minutes read"
                then "1 minute read ("
                else (show m) ++ " minutes read ("
            ++ (show w) ++ " words" ++
            -- Don't mention images if there are none
            if i /= 0 then " and " ++ (show i) ++ "images)" else ")"

addTime :: ReadTime -> ReadTime -> ReadTime
addTime (ReadTime mw mi) (ReadTime nw ni) = ReadTime (mw + nw) (mi + ni)

minutesReadTime :: ReadTime -> Int
-- Read time (in minutes) is the number of words `div` the avg number of WPM (275)
-- plus
-- Time for looking at pictures. Medium calculates 12 seconds for the
-- first, 11 for the second, 10 for the third picture ... (`reverse
-- [3..12]`) and 3 seconds for every picture after the 10th (repeat 3)
minutesReadTime (ReadTime w i) = (w `div` 275) + ((sum $ take i $ reverse [3..12] ++ repeat 3) `div` 60)

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

apply :: (Node -> Node) -> Node -> Node
apply f n = let (Node p t ns) = f n in
    Node p t $ map (apply f) ns

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


renderContent :: Node -> Html ()
renderContent = toHtmlRaw . nodeToHtml [optSmart, optNormalize]

htmlHead :: Text -> Text -> [Text] -> Html ()
htmlHead author desc keywords = head_ $ do
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/default.css"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/fontawesome.css"]
    meta_ [charset_ "utf-8"]
    m "generator" "hibachi-1.0"
    m "referrer" "no-referrer"
    m "HandheldFriendly" "True"
    m "viewport" "width=device-width, initial-scale=1.0"

    m "author" author
    m "description" desc
    m "keywords" $ intercalate "," keywords
  where
    m _ "" = return ()
    m a b = meta_ [name_ a, content_ b]

htmlBody :: Html () -> Html ()
htmlBody c = body_ $ let togglenav = "toggle-nav" in do

    input_ [id_ togglenav, class_ togglenav, type_ "checkbox"]
    div_ [class_ "mobile-bar"] $ label_ [for_ togglenav] ""
    nav_ [class_ "navbar"] $ do
        header_ $ a_ [class_ "navlink", href_ "/"] $ img_ [class_ "logo", src_ "/images/logo.svg"]
        ul_ [id_ "navleft"] $ do
            a_ (navlink "/about") $ li_ ( span_ (navicon "far fa-question-circle") "" <> "About" )
            a_ (navlink "/projects") $ li_ ( span_ (navicon "fas fa-terminal") "" <> "Projects" )
        div_ [class_ "spacer"] ""
        ul_ [id_ "navright"] $ do
            a_ (navlink "/feed.xml") $ li_ ( span_ (navicon "fas fa-rss") "" <> "RSS" )
            a_ (navlink "https://github.com/dequbed") $ li_ ( span_ (navicon "fab fa-github") "" <> "Github" )
            a_ (navlink "https://mastodon.chaosfield.at/@dequbed") $ li_ ( span_ (navicon "fab fa-mastodon") "" <> "Mastodon" )

    main_ $ c

    footer_ [id_ "footer"] $ span_ [class_ "license"] $ do {
        "Unless otherwise noted all content is licensed under a";
        a_ [rel_ "license", href_ "http://creativecommons.org/licenses/by-sa/4.0/"]
            "Creative Commons Attribution-ShareAlike 4.0 International License";
    }
  where
    navicon name = [class_ $ "navicon " <> name]
    navlink href = [class_ "navlink", href_ href]

parToH1' :: NodeType -> NodeType
parToH1' PARAGRAPH = HEADING 1
parToH1' nt = nt

parToH1 :: Node -> Node
parToH1 (Node p t ns) = Node p (parToH1' t) ns

postHeader :: Text -> ReadTime -> Html ()
postHeader title time = do
    header_ $ do
        toHtmlRaw $ nodeToHtml [] $ apply parToH1 $ commonmarkToNode [optSmart] title
        div_ [class_ "readtime"] $ do
            span_ [class_ "far fa-clock fa-sm"] ""
            toHtml $ " " ++ show time

postShortHeader :: Text -> Html ()
postShortHeader = header_ . toHtmlRaw . nodeToHtml [] . apply parToH1 . commonmarkToNode [optSmart]

postFooter :: ZonedTime -> [Text] -> Text -> Html ()
postFooter posted tags author = do
    footer_ [class_ "post-footer"] $ do
        "Posted "
        time_ [datetime_ (pack $ formatTime defaultTimeLocale "%Y-%m-%dT%T%z" posted)] 
                $ toHtml (pack $ formatTime defaultTimeLocale "%d. %b %Y %R %Z" posted)
        " in"
        ul_ [class_ "tags"] $ mapM_ (li_ [class_ ("tag")] . toHtml) tags
        "by "
        span_ [class_ "author"] $ toHtml author
