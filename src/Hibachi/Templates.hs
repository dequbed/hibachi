module Hibachi.Templates
    ( postHeader
    , postShortHeader
    , postFooter
    , htmlHead
    , htmlBody
    , apply
    , renderContent
    )
    where

import Hibachi.ReadTime
import Hibachi.Post

import CMark
import Lucid

import Data.Text (Text, intercalate, pack, words, lines, unlines)
import Data.Text.Encoding (encodeUtf8)

import Data.Time
import Data.Time.Clock
import Data.Time.Format

import Data.Maybe

import System.FilePath.Posix

renderPost :: Post -> Html ()
renderPost (PlainPost p) = renderPost' p
renderPost (Story _ this _) = renderPost' this
renderPost' p = do
    doctype_ 
    html_ [lang_ "en"] $ do
        htmlHead (postAuthor p) (nodeToHtml [optSmart, optNormalize] $ postAbstract p) (postKeywords p)
        htmlBody $ do
            article_ [class_ "post"] $ do
                postHeader (postTitle p) (postReadTime p)
                renderContent $ (postContent p)
                postFooter (postPostedTime p) (postTags p) (postAuthor p)

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


renderIndex :: [PostCommon] -> Html ()
renderIndex ps = do
    doctype_
    html_ [lang_ "en"] $ do
        htmlHead mempty mempty mempty :: Html ()
        htmlBody $ mapM_ renderShortPostlink ps

renderPostlink :: PostCommon -> Html ()
renderPostlink p = a_ [class_ "postlink", href_ (pack $ postLinkPath p)] $ article_ [class_ "post"] $ do
    postHeader (postTitle p) (postReadTime p)
    toHtmlRaw $ nodeToHtml [optSmart, optNormalize] (postAbstract p)
    postFooter (postPostedTime p) (postTags p) (postAuthor p)

renderShortPostlink :: PostCommon -> Html ()
renderShortPostlink p = a_ [class_ "postlink", href_ (pack $ postLinkPath p)] $ article_ [class_ "post"] $ do
    postShortHeader (postTitle p)
    toHtmlRaw $ nodeToHtml [optSmart, optNormalize] (postAbstract p)
    postFooter (postPostedTime p) (postTags p) (postAuthor p)

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

postHeader :: Node -> ReadTime -> Html ()
postHeader title time = do
    header_ $ do
        toHtmlRaw $ nodeToHtml [] $ apply parToH1 title
        div_ [class_ "readtime"] $ do
            span_ [class_ "far fa-clock fa-sm"] ""
            toHtml $ " " ++ show time

postShortHeader :: Node -> Html ()
postShortHeader = header_ . toHtmlRaw . nodeToHtml [] . apply parToH1

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
