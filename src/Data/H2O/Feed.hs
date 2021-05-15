module Data.H2O.Feed
    ( genFeed
    , renderFeed
    , toEntry
    ) where

import CMark

import qualified Prelude.Text as T
import Data.Text.Lazy (toStrict)
import Text.Feed.Types

import Text.XML (def, rsPretty)
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Export as Export (textFeedWith)

import Data.H2O.Types

genFeed :: Atom.Feed
genFeed = Atom.nullFeed
    "http://dequbed.space/feed.xml"
    (Atom.TextString "Dequbeds Blog")
    "2021-05-15"

renderFeed :: Atom.Feed -> Maybe Text
renderFeed = fmap toStrict . Export.textFeedWith def{rsPretty = True} . AtomFeed

toEntry :: Text -> Post -> Atom.Entry
toEntry link post =
    (Atom.nullEntry
        link
        (Atom.HTMLString $ nodeToHtml [] $ post^.title)
        (T.pack $ show $ post^.posted))
    { Atom.entryAuthors = [Atom.nullPerson { Atom.personName = post^.author }]
    , Atom.entryLinks = [Atom.nullLink link]
    , Atom.entryContent = Just (Atom.HTMLContent $ nodeToHtml [] $ post^.content)
    }
