module Templates
    ( renderPostText
    , renderIndex
    , aboutTemplate
    , renderTagIndex
    , feedTemplateTxt
    ) where

import Prelude hiding (for_)
import System.FilePath

import Data.H2O
import Data.H2O.Post
import Data.H2O.ReadTime
import Data.H2O.Templates

import Data.Time

import Data.Text.Lazy (toStrict)
import Data.Text (Text, intercalate, pack)

htmlPre inner = do
    -- HTML5 specific doctype
    doctype_
    html_ [lang_ "en"] inner

meta :: Text -> Text -> Html ()
-- Default cases for empty value or key
meta "" _ = return ()
meta _ "" = return ()
meta key value = meta_ [name_ key, content_ value]

whenMaybe :: Applicative f => (a -> f ()) -> Maybe a -> f ()
whenMaybe = maybe (pure ())

-- Write a HTML head tag with some common stuff and additional HTML
htmlHeadM :: Html () -> Html ()
htmlHeadM rest = head_ $ do
    -- The stylesheets are constant and required for pretty much all sites
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/default.css"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/fontawesome.css"]
    -- Code highlighting
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/code.css"]

    meta_ [charset_ "utf-8"]
    -- TODO: Read out the version of hibachi used.
    meta "generator" "hibachi-1.0"
    meta "referrer" "no-referrer"
    meta "HandheldFriendly" "True"
    meta "viewport" "width=device-width, initial-scale=1.0"
    rest

-- A Simple HTML head with a few more metadata set
htmlHead :: Maybe Text -> Maybe Text -> [Text] -> Html ()
htmlHead author desc keywords = htmlHeadM $ do
    -- Any of those may be Nothing so only set a meta if it is a Just
    whenMaybe (meta "author") author
    whenMaybe (meta "description") desc
    -- Unset keywords are an empty list which will return an empty string and thus not do anything
    meta "keywords" $ intercalate "," keywords

-- The default HTML body with navbar and footer
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
            a_ (navlink "/feed.html") $ li_ ( span_ (navicon "fas fa-rss") "" <> "RSS" )
            a_ (navlink "https://github.com/dequbed") $ li_ ( span_ (navicon "fab fa-github") "" <> "Github" )
            a_ (navlink "https://mastodon.chaosfield.at/@dequbed") $ li_ ( span_ (navicon "fab fa-mastodon") "" <> "Mastodon" )
    main_ c
    footer_ [id_ "footer"] $ span_ [class_ "license"] $ do {
        "Unless otherwise noted all content is licensed under a";
        a_ [rel_ "license", href_ "http://creativecommons.org/licenses/by-sa/4.0/"]
            "Creative Commons Attribution-ShareAlike 4.0 International License";
    }
  where
    navicon name = [class_ $ "navicon " <> name]
    navlink href = [class_ "navlink", href_ href]


-- A list view with some optional HTML above the listing
listView :: Html () -> [(FilePath, Post)] -> Html ()
listView preamble posts = htmlPre $ do
    htmlHeadM (return ())
    htmlBody $ do
        preamble
        mapM_ (uncurry renderPostlink) posts

-- Template for the main index
indexTemplate :: [(FilePath, Post)] -> Html ()
indexTemplate = listView (return ())

-- Tempalate for the tags view
tagsTemplate :: Text -> [(FilePath, Post)] -> Html ()
tagsTemplate tag = listView (p_ [] $ toHtmlRaw $ tag)

renderPostlink :: FilePath -> Post -> Html ()
renderPostlink path post =
    article_ [class_ "post"] $ do
        postHeaderF (a_ [href_ (pathToLink path)] $ toHtmlRaw $ nodeToHtml [] $ apply parToH1 $ post^.title) (post^.title) (post^.readtime) 
        div_ [class_ "abstract"] $ do
            renderNode [] (post^.abstract)
            a_ [href_ (pathToLink path)] "More…"
        postFooter (post^.posted) (post^.tags) (post^.author)

renderPost :: Post -> Html ()
renderPost post = renderPost'
        (post^.author)
        (post^.abstract)
        []
        (post^.title)
        (calculateReadTime $ post^.content)
        (post^.content)
        (post^.posted)
        (post^.tags)
    where
    renderPost' author abstract keywords title readtime content postedTime tags =
        htmlPre $ do
            htmlHead (Just author) (Just $ nodeToHtml [optSmart, optNormalize] abstract) keywords
            htmlBody $
                article_ [class_ "post"] $ do
                    postHeader title readtime
                    renderNode [] $ apply dropHeadingLevel content
                    postFooter postedTime tags author

renderAbout :: Node -> Html ()
renderAbout about =
    htmlPre $ do
        htmlHead (Just "Gregor 'dequbed' Reitzenstein") (Just "") ["Blog", "dequbed"]
        htmlBody $
            div_ [class_ "post"]
                $ renderNode []
                $ apply dropHeadingLevel about

aboutTemplate :: Text -> Text
aboutTemplate = renderHtmlT . renderAbout . readCM

postTemplate :: Meta -> Text -> Either Text Text
postTemplate m t = renderHtmlT . renderPost <$> readPost m t

storyTemplate :: Story -> Text
storyTemplate = pack . show

feedTemplateTxt :: Text
feedTemplateTxt = renderHtmlT feedTemplate

feedTemplate :: Html ()
feedTemplate = htmlPre $ do
    htmlHeadM (return ())
    htmlBody $
        article_ [class_ "post"] $ do
            span_ [class_ "fas fa-hard-hat"] ""
            p_ "Work in progress"

renderIndex :: [(FilePath, Post)] -> Text
renderIndex = renderHtmlT . indexTemplate

renderTagIndex :: Text -> [(FilePath, Post)] -> Text
renderTagIndex t p = renderHtmlT $ tagsTemplate t p

renderPostText :: Post -> Text
renderPostText = renderHtmlT . renderPost

pathToLink :: FilePath -> Text
pathToLink = pack . dropTrailingPathSeparator . makeRelative "/"

parToH1' :: NodeType -> NodeType
parToH1' PARAGRAPH = HEADING 1
parToH1' nt        = nt

parToH1 :: Node -> Node
parToH1 (Node p t ns) = Node p (parToH1' t) ns

postHeader :: Node -> ReadTime -> Html ()
postHeader title = postHeaderF (toHtmlRaw $ nodeToHtml [] $ apply parToH1 title) title

postHeaderF :: Html () -> Node -> ReadTime -> Html ()
postHeaderF h title time =
    header_ $ do
        h
        div_ [class_ "readtime"] $ do
            span_ [class_ "far fa-clock fa-sm"] ""
            toHtml $ " " ++ show time

postShortHeader :: Node -> Html ()
postShortHeader = header_ . toHtmlRaw . nodeToHtml [] . apply parToH1

postFooter :: UTCTime  -> [Text] -> Text -> Html ()
postFooter posted tags author =
    footer_ [class_ "post-footer"] $ do
        "Posted "
        time_ [datetime_ (pack $ formatTime defaultTimeLocale "%Y-%m-%dT%T%z" posted)]
                $ toHtml (pack $ formatTime defaultTimeLocale "%d. %b %Y %R" posted)
        " in"
        ul_ [class_ "tags"] $ mapM_ (\t -> li_ [class_ "tag"] $ a_ [href_ $ "/tags/" <> t] $ toHtml t) tags
        "by "
        span_ [class_ "author"] $ toHtml author
