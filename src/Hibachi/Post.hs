module Hibachi.Post
    ( wrapPost
    )
    where

import Lucid
import Data.Text (Text, intercalate)

wrapPost :: Text -> Html ()
wrapPost t = wrapPost' $ (toHtmlRaw t)

wrapPost' :: Html () -> Html ()
wrapPost' p = html_ $ do
    htmlHead
        "Gregor 'dequbed' Reitzenstein"
        "My Blog"
        ["Blog", "dequbed"]
    htmlBody p

htmlHead :: Text -> Text -> [Text] -> Html ()
htmlHead author desc keywords = head_ $ do
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/styles/default.css"]
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
htmlBody p = body_ $ do
    let togglenav = "toggle-nav"
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

    main_ $ article_ [class_ "post"] p

    footer_ [id_ "footer"] $ span_ [class_ "license"] $ do {
        "Unless otherwise noted all content is licensed under a";
        a_ [rel_ "license", href_ "http://creativecommons.org/licenses/by-sa/4.0/"]
            "Creative Commons Attribution-ShareAlike 4.0 International License";
    }
  where
    navicon name = [class_ $ "navicon fa fa-" <> name]
    navlink href = [class_ "navlink", href_ href]
