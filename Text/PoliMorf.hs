{-# LANGUAGE BangPatterns #-}

module Text.PoliMorf
( Entry (..)
, parsePoliMorf
) where

import qualified Data.Text as T
import Data.List.Split (wordsBy)

data Entry = Entry
    { form :: T.Text
    , base :: T.Text
    , tag  :: T.Text }

-- | Get a list of pairs (form, lemma) from a PoliMorf string.
parsePoliMorf :: String -> [Entry]
parsePoliMorf = map parsePoliRow . lines 

-- | Get a (form, lemma) pair from a PoliMorf row.
parsePoliRow :: String -> Entry
parsePoliRow row =
    let [!form, !base, !tag] = map T.pack (wordsBy (=='\t') row)
    in  Entry form base tag
