module Data.H2O.Templates
    ( module Lucid
    , module CMark
    , module CMark.Lucid

    , apply
    , dropHeadingLevel

    , readCM
    , renderHtmlT
    )
    where

import Data.Text.Lazy (toStrict)
import Data.Text (Text)


import CMark
import CMark.Lucid
import Lucid

apply :: (Node -> Node) -> Node -> Node
apply f n = let (Node p t ns) = f n in
    Node p t $ map (apply f) ns

wrap :: (NodeType -> NodeType) -> Node -> Node
wrap f (Node p t ns) = Node p (f t) ns

dropHeadingLevel' :: NodeType -> NodeType
dropHeadingLevel' (HEADING 6) = STRONG
dropHeadingLevel' (HEADING x) = HEADING (x+1)
dropHeadingLevel' nt          = nt

dropHeadingLevel :: Node -> Node
dropHeadingLevel = wrap dropHeadingLevel'


readCM :: Text -> Node
readCM = commonmarkToNode [optSmart]

renderHtmlT :: Html () -> Text
renderHtmlT = toStrict . renderText
