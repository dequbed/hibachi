module Templates where

import Prelude hiding (for_)

import Data.H2O
import Data.H2O.Post
import Data.H2O.ReadTime
import Data.H2O.Templates

import Data.Time

import Data.Text.Lazy (toStrict)
import Data.Text (Text, intercalate, pack)

aboutTemplate :: Text -> Text
aboutTemplate = renderHtmlT . renderAbout . readCM

postTemplate :: Meta -> Text -> Either Text Text
postTemplate m t = renderHtmlT . renderPost <$> readPost m t

storyTemplate :: Story -> Text
storyTemplate = pack . show

indexTemplate :: [Post] -> Text
indexTemplate = renderHtmlT . renderIndex

renderIndex :: [Post] -> Html ()
renderIndex ps = htmlPre $ do
        htmlHeadM (return ())
        htmlBody $ mapM_ renderPostlink ps

htmlPre f = do
    doctype_
    html_ [lang_ "en"] f

renderPost :: Post -> Html ()
renderPost post = renderPost'
        (post^.author)
        (post^.abstract)
        []
        (post^.title)
        (calculateReadTime $ post^.content)
        (post^.content)
        (post^.posted)
        []
    where
    renderPost' author abstract keywords title readtime content postedTime tags =
        htmlPre $ do
            htmlHead author (nodeToHtml [optSmart, optNormalize] abstract) keywords
            htmlBody $
                article_ [class_ "post"] $ do
                    postHeader title readtime
                    renderNode [] content
                    postFooter postedTime tags author

renderAbout :: Node -> Html ()
renderAbout about =
    htmlPre $ do
        htmlHead "Gregor 'dequbed' Reitzenstein" "" ["Blog", "dequbed"]
        htmlBody $
            div_ [class_ "post"]
                $ renderNode []
                $ apply dropHeadingLevel about

renderPostlink :: Post -> Html ()
renderPostlink post =
    a_ [class_ "postlink", href_ ""] $ article_ [class_ "post"] $ do
        postHeader (post^.title) (post^.readtime)
        renderNode [] (post^.abstract)
        postFooter (post^.posted) (post^.tags) (post^.author)

-- renderIndex :: [PostCommon] -> Html ()
-- renderIndex ps = do
--     doctype_
--     html_ [lang_ "en"] $ do
--         htmlHead mempty mempty mempty :: Html ()
--         htmlBody $ mapM_ renderShortPostlink ps
-- 
-- renderPostlink :: PostCommon -> Html ()
-- renderPostlink p =
--     a_ [class_ "postlink", href_ (pack $ postLinkPath p)] $ article_ [class_ "post"] $ do
--         postHeader (postTitle p) (postReadTime p)
--         toHtmlRaw $ nodeToHtml [optSmart, optNormalize] (postAbstract p)
--         postFooter (postPostedTime p) (postTags p) (postAuthor p)
-- 
-- renderShortPostlink :: PostCommon -> Html ()
-- renderShortPostlink p =
--     a_ [class_ "postlink", href_ (pack $ postLinkPath p)] $ article_ [class_ "post"] $ do
--         postShortHeader (postTitle p)
--         toHtmlRaw $ nodeToHtml [optSmart, optNormalize] (postAbstract p)
--         postFooter (postPostedTime p) (postTags p) (postAuthor p)

htmlHeadM :: Html () -> Html ()
htmlHeadM i = head_ $ do
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/default.css"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/fontawesome.css"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/code.css"]
    meta_ [charset_ "utf-8"]
    m "generator" "hibachi-1.0"
    m "referrer" "no-referrer"
    m "HandheldFriendly" "True"
    m "viewport" "width=device-width, initial-scale=1.0"

    i
  where
    m "" _ = return ()
    m _ "" = return ()
    m a b  = meta_ [name_ a, content_ b]


htmlHead :: Text -> Text -> [Text] -> Html ()
htmlHead author desc keywords = htmlHeadM $ do
    m "author" author
    m "description" desc
    m "keywords" $ intercalate "," keywords
  where
    m _ "" = return ()
    m a b  = meta_ [name_ a, content_ b]

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

    main_ c

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
parToH1' nt        = nt

parToH1 :: Node -> Node
parToH1 (Node p t ns) = Node p (parToH1' t) ns

postHeader :: Node -> ReadTime -> Html ()
postHeader title time =
    header_ $ do
        toHtmlRaw $ nodeToHtml [] $ apply parToH1 title
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
        ul_ [class_ "tags"] $ mapM_ (li_ [class_ "tag"] . toHtml) tags
        "by "
        span_ [class_ "author"] $ toHtml author
