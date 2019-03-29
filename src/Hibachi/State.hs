{-# LANGUAGE DeriveGeneric #-}
module Hibachi.State
    ( saveState
    , loadState
    , isCompatible
    , verCompat

    , Version
    , State(..)
    )
    where

import GHC.Generics

import Prelude hiding (readFile, writeFile)

import Data.Store
import Data.Text
import Data.ByteString (writeFile, readFile)

type Version = (Int, Int, Int)

data State = State
           { version :: Version
           , lastCommit :: Text
           } deriving (Eq, Show, Generic)
instance Store State

isCompatible :: State -> State -> Bool
isCompatible a b = (version a) `verCompat` (version b)

verCompat :: Version -> Version -> Bool
verCompat (0,_,_) _ = False
verCompat _ (0,_,_) = False
verCompat (a,b,_) (x,y,_) = (a==x) && (b==y)

saveState :: FilePath -> State -> IO ()
saveState path = (writeFile path) . encode

loadState :: FilePath -> IO (Either PeekException State)
loadState path = decode <$> readFile path
