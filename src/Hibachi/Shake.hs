{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hibachi.Shake
    where

import Development.Shake
import Development.Shake.Rule
import Development.Shake.Classes

import Hibachi.Post

newtype PostR = PostR String
    deriving (Show, Eq, Hashable, Binary, NFData)
type instance RuleResult PostR = Post

data PostRule = PostRule PostR (Action ())

postRule :: String -> Action () -> Rules ()
postRule title act = addUserRule $ PostRule (PostR title) act

postNeed :: String -> Action Post
postNeed = apply1 . PostR

needAllPosts :: Action [Post]
needAllPosts = undefined

