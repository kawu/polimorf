{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- | The module provides functionality for manipulating PoliMorf, the
-- morphological dictionary for Polish.

module Data.PoliMorf
( 
-- * Types
  Form
, Base
, POS
, MSD
, Tag
, Cat
, Entry (..)
, split
, pos
, msd
, atomic

-- * Parsing
, readPoliMorf
, parsePoliMorf
) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

-- | A word form.
type Form = T.Text

-- | A base form.
type Base = T.Text

-- | A part of speech.
type POS  = T.Text

-- | A morphosyntactic description 
type MSD  = T.Text

-- | A morphosyntactic tag. (Tag = POS + MSD)
type Tag  = T.Text

-- | A semantic category.  It will be set to "" when there is
-- no category assigned to a particular PoliMorf entry.
type Cat  = T.Text

-- | An entry from the PoliMorf dictionary.
data Entry = Entry
    { form :: !Form
    , base :: !Base
    , tag  :: !Tag
    , cat  :: !Cat }
    deriving (Eq, Ord, Show, Read)

-- | Split tag.
split :: Tag -> (POS, MSD)
split = second (T.drop 1) . T.break (==':')

-- | Entry POS.
pos :: Entry -> POS
pos = fst . split . tag

-- | Entry MSD.
msd :: Entry -> MSD
msd = snd . split . tag

-- | Is the entry an atomic one?  More precisely, we treat all negative
-- forms starting with ''nie'' and all superlatives starting with ''naj''
-- as non-atomic entries.
atomic :: Entry -> Bool
atomic x
    | "sup" `T.isInfixOf` tag x && "naj" `T.isPrefixOf` form x = False
    | "neg" `T.isInfixOf` tag x && "nie" `T.isPrefixOf` form x = False
    | otherwise = True

-- | Read the PoliMorf from the file.
readPoliMorf :: FilePath -> IO [Entry]
readPoliMorf path = parsePoliMorf <$> L.readFile path

-- | Parse the PoliMorf into a list of entries.
parsePoliMorf :: L.Text -> [Entry]
parsePoliMorf = map parsePoliRow . L.lines 

-- | Get an entry pair from a PoliMorf row.
parsePoliRow :: L.Text -> Entry
parsePoliRow row = case map L.toStrict (L.split (=='\t') row) of
    _form : _base : _tag : rest -> Entry _form _base _tag $ case rest of
        []       -> ""
        (_cat:_) -> _cat
    _   -> error $ "parsePoliRow: invalid row \"" ++ L.unpack row ++ "\""
