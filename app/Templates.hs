module Templates where

import Data.H2O

import Data.Text.Lazy (toStrict)
import Data.Text (Text)

import CMark.Lucid
import Lucid

readCM :: Text -> Node
readCM = commonmarkToNode [optSmart]

renderHtmlT :: Html () -> Text
renderHtmlT = toStrict . renderText

aboutTemplate :: Text -> Text
aboutTemplate = renderHtmlT . postTemplate' . renderNode [] . readCM

commonHeader = head_ $ do
    link_ [href_ "/css/default.css", type_ "text/css", rel_ "stylesheet"]
    link_ [href_ "/css/default.css", type_ "text/css", rel_ "stylesheet"]
    link_ [href_ "/css/fontawesome.css", type_ "text/css", rel_ "stylesheet"]
    meta_ [charset_ "utf-8"]
    meta_ [content_ "hibachi-1.0", name_ "generator"]
    meta_ [content_ "no-referrer", name_ "referrer"]
    meta_ [content_ "True", name_ "HandheldFriendly"]
    meta_ [content_ "width_ device-width, initial-scale_ 1.0", name_ "viewport"]
    meta_ [content_ "Gregor &#39;dequbed&#39; Reitzenstein", name_ "author"]
    meta_ [content_ "Blog,dequbed", name_ "keywords"]

commonFooter = footer_ [id_ "footer"] $
    span_ [class_ "license"] $ do
        toHtml ("Unless otherwise noted all content is licensed under a" :: Text)
        a_ [href_ "http://creativecommons.org/licenses/by-sa/4.0/", rel_ "license"] $
              toHtml ("Creative Commons Attribution-ShareAlike 4.0 International License" :: Text)

postTemplate' :: Html () -> Html ()
postTemplate' i = html_ $ do
    commonHeader
    body_ $ do
        main_ $
            div_ [class_ "post"] 
                i
        commonFooter
