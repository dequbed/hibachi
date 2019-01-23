module Hibachi.Post
    ( Post
    , buildPost
    , renderPost
    )
    where

import Lucid
import Data.Text (Text, intercalate, pack)

import Data.Time
import Data.Time.Format

import CMark

data Post = Post
          { author :: Text
          , desc :: Text
          , keywords :: [Text]

          , title :: Text
          , preadTime :: NominalDiffTime

          , content :: Text

          , posted :: UTCTime
          , tags :: [Text]
          } deriving (Show, Eq)

buildPost :: Text -> Text -> [Text] -> UTCTime -> [Text] -> Text -> Post
buildPost author desc keywords posted tags content = do
    Post author desc keywords (extractTitle content) (calculateReadTime content) content posted tags

extractTitle :: Text -> Text
extractTitle _ = ""

calculateReadTime :: Text -> NominalDiffTime
calculateReadTime _ = nominalDay

renderPost :: Post -> Html ()
renderPost p = do
        doctype_ 
        html_ [lang_ "en"] $ do
            htmlHead (author p) (desc p) (keywords p)
            htmlBody $ do
                postHeader (title p) (preadTime p)
                renderContent (content p)
                postFooter (posted p) (tags p) (author p)

renderContent :: Text -> Html ()
renderContent = toHtmlRaw . commonmarkToHtml [optSmart, optNormalize]

htmlHead :: Text -> Text -> [Text] -> Html ()
htmlHead author desc keywords = head_ $ do
    link_ [rel_ "stylesheet", type_ "text/css", href_ "./default.css"]
    meta_ [charset_ "utf-8"]
    m "generator" "hibachi-1.0"
    m "referrer" "no-referrer"
    m "HandheldFriendly" "True"
    m "viewport" "width=device-width, initial-scale=1.0"

    m "author" author
    m "description" desc
    m "keywords" $ intercalate "," keywords
  where
    m a b = meta_ [name_ a, content_ b]

htmlBody :: Html () -> Html ()
htmlBody p = body_ $ let togglenav = "toggle-nav" in do

    input_ [id_ togglenav, class_ togglenav, type_ "checkbox"]
    div_ [class_ "mobile-bar"] $ label_ [for_ togglenav] ""
    nav_ [class_ "navbar"] $ do
        header_ $ a_ [class_ "navlink", href_ "/"] $ img_ [class_ "logo", src_ "/images/logo.svg"]
        ul_ [id_ "navleft"] $ do
            a_ (navlink "/about") $ li_ ( span_ (navicon "question-circle") "" <> "About" )
            a_ (navlink "/projects") $ li_ ( span_ (navicon "terminal") "" <> "Projects" )
        div_ [class_ "spacer"] ""
        ul_ [id_ "navright"] $ do
            a_ (navlink "/feed.xml") $ li_ ( span_ (navicon "feed") "" <> "RSS" )
            a_ (navlink "https://github.com/dequbed") $ li_ ( span_ (navicon "github") "" <> "Github" )
            a_ (navlink "https://twitter.com/dequbed") $ li_ ( span_ (navicon "twitter") "" <> "Twitter" )

    main_ $ article_ [class_ "post"] $ p

    footer_ [id_ "footer"] $ span_ [class_ "license"] $ do {
        "Unless otherwise noted all content is licensed under a";
        a_ [rel_ "license", href_ "http://creativecommons.org/licenses/by-sa/4.0/"]
            "Creative Commons Attribution-ShareAlike 4.0 International License";
    }
  where
    navicon name = [class_ $ "navicon fa fa-" <> name]
    navlink href = [class_ "navlink", href_ href]

postHeader :: Text -> NominalDiffTime -> Html ()
postHeader title time = do
    header_ $ do
        h1_ $ toHtml title
        div_ [class_ "readtime"] $ do
            span_ [class_ "fa fa-clock-o fa-sm"] ""
            toHtml $ show time

postFooter :: UTCTime -> [Text] -> Text -> Html ()
postFooter posted tags author = do
    footer_ [class_ "post-footer"] $ do
        "Posted"
        time_ [datetime_ (pack $ show posted)] $ toHtml (pack $ formatTime defaultTimeLocale "%d. %b %Y %R" posted)
        "in"
        ul_ [class_ "tags"] $ mapM_ (li_ [class_ ("tag")] . toHtml) tags
        "by"
        toHtml author
