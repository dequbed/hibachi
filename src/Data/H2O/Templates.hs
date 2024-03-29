module Data.H2O.Templates
    ( module Lucid
    , module CMark
    , module CMark.Lucid

    , apply
    , hibachiFilters
    , hibachiFilters'

    , readCM
    , renderHtmlT
    )
    where

import Data.Text.Lazy (toStrict)
import Data.Text (Text, pack)

--import Control.Lens
--import Control.Lens.Setter
--
import Data.H2O.Types (Post, content)
import Data.H2O.Filters (filterLfs)

import CMark
import CMark.Lucid
import Lucid
import Skylighting hiding (formatHtmlBlock)
import Skylighting.Format.HTML.Lucid (formatHtmlBlock)

import Development.Shake (Action)

apply :: (Node -> Node) -> Node -> Node
apply f n = let (Node p t ns) = f n in
    Node p t $ map (apply f) ns

wrap :: (NodeType -> NodeType) -> Node -> Node
wrap f (Node p t ns) = Node p (f t) ns

dropHeadingLevel :: NodeType -> NodeType
dropHeadingLevel (HEADING x) | x >= 6 = STRONG
dropHeadingLevel (HEADING x) = HEADING (x+1)
dropHeadingLevel nt = nt

h2oTokenize :: Text -> Text -> Either String [SourceLine]
h2oTokenize name code = do
    syn <- maybe (Left "Can't find that syntax") Right (lookupSyntax name defaultSyntaxMap)
    tokenize (TokenizerConfig defaultSyntaxMap False) syn code

hilightCode :: Node -> Node
hilightCode n@(Node p (CODE_BLOCK info code) _) = case h2oTokenize info code of
      Left e -> traceShow e n
      Right lines -> (\t -> Node p (HTML_BLOCK t) []) $ toStrict $ renderText $ formatHtmlBlock defaultFormatOpts lines
hilightCode n = n

hibachiFilters :: Node -> Action Node
hibachiFilters node = do
    node <- filterLfs node
    return $ apply (hilightCode . wrap dropHeadingLevel) node

hibachiFilters' :: Post -> Action Post
hibachiFilters' post = do
    node <- hibachiFilters (post^.content)
    return $ post & content .~ node

readCM :: Text -> Node
readCM = commonmarkToNode [optSmart]

renderHtmlT :: Html () -> Text
renderHtmlT = toStrict . renderText
