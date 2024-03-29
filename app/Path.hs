module Path 
    ( writeFile
    , H2OPath(..)
    ) where

import Prelude.Text (unpack)
import Prelude.FilePath
import Prelude.Directory
import Development.Shake

import Data.H2O.Shake (getOutPrefix)
import Data.H2O.Shake.Utils (writeFileText)

data H2OPath 
  = GitPath Text
  | HtmlPath Text

toFilePath :: H2OPath -> FilePath
toFilePath (GitPath t) = unpack t
toFilePath (HtmlPath t) = unpack t

-- Write a file, taking an prefix from ShakeExtra (usually "out")
writeFile :: FilePath -> Text -> Action ()
writeFile = writeFileText
