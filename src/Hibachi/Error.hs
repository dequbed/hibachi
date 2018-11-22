module Hibachi.Error
    ( Result
    , HibachiError(..)
    ) where

import Prelude hiding ((<>))

import Data.Text (Text)
import qualified Data.Text as T

type Result r = Either HibachiError r

data HibachiError
    = Generic Text
    | MetaLookupFailed Text
    | InvalidTitleType
    | InvalidAbstractType
    | InvalidTagsType
    | InvalidTagType
    deriving (Eq, Show)


toHumanReadable :: HibachiError -> Text
toHumanReadable = s

s (MetaLookupFailed f) = "Required field \"" <> f <> "\" not set in Metadata."
s InvalidTitleType = "Document title is of an invalid datatype."
s InvalidAbstractType = "Document abstract is of an invalid datatype."
s InvalidTagsType = "Document tags are of an invalid datatype. It must be a List."
s InvalidTagType = "One or more Document tags are of an invalid datatype. They must be Strings."

(<>) = T.append
