{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Char (isSpace, toUpper)
import qualified Data.Text as T (Text, all, cons, pack, toLower, uncons, unpack)
import Text.Read (readMaybe)

-- | Like @'show'@, but returns a 'T.Text' instead of a 'String'.
tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

-- | Like @'readMaybe'@, but takes 'T.Text' as input instead of 'String'.
treadMaybe :: (Read a) => T.Text -> Maybe a
treadMaybe = readMaybe . T.unpack

allIsSpace :: T.Text -> Bool
allIsSpace = T.all isSpace

capitalize :: T.Text -> T.Text
capitalize t =
  case T.uncons t of
    Nothing -> ""
    Just (c, rest) -> T.cons (toUpper c) (T.toLower rest)
