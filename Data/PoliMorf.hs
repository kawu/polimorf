{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}

-- | The module provides functionality for manipulating PoliMorf, the
-- morphological dictionary for Polish. Apart from IO utilities there
-- is a 'merge' function which can be used to merge the PoliMorf with
-- another dictionary resources.

module Data.PoliMorf
( 
-- * Types
  Form
, Base
, Tag
, Entry (..)

-- * Parsing
, readPoliMorf
, parsePoliMorf

-- * Merging
, BaseMap
, mkBaseMap
, RelCode (..)
, Origin (..)
, merge
) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (catMaybes)
import Data.Monoid (mappend)
import Data.List (foldl')
import Data.Binary (Binary, get, put)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

-- | A form.
type Form = T.Text

-- | A base form.
type Base = T.Text

-- | A morphosyntactic tag.
type Tag  = T.Text

-- | A category.
type Cat  = T.Text

-- | An entry from the PoliMorf dictionary.
data Entry = Entry
    { form :: !Form
    , base :: !Base
    , tag  :: !Tag
    , cat  :: !Cat }
    deriving (Eq, Ord, Show, Read)

-- | Read the PoliMorf from the file.
readPoliMorf :: FilePath -> IO [Entry]
readPoliMorf path = parsePoliMorf <$> L.readFile path

-- | Parse the PoliMorf into a list of entries.
parsePoliMorf :: L.Text -> [Entry]
parsePoliMorf = map parsePoliRow . L.lines 

-- | Get an entry pair from a PoliMorf row.
parsePoliRow :: L.Text -> Entry
parsePoliRow row = case map L.toStrict (L.split (=='\t') row) of
    [_form, _base, _tag, _cat] -> Entry _form _base _tag _cat
    _   -> error $ "parsePoliRow: invalid row \"" ++ L.unpack row ++ "\""

-- | A map from forms to their possible base forms (there may be many since
-- the form may be a member of multiple lexemes).
type BaseMap = M.Map Form (S.Set Base)

-- | Make the base map from the list of entries.
mkBaseMap :: [Entry] -> BaseMap
mkBaseMap = M.fromListWith S.union . map ((,) <$> form <*> S.singleton . base)

-- | Reliability information: how did we assign a particular label to
-- a particular word form.
data RelCode
    = Exact     -- ^ Label assigned in a direct manner
    | ByBase    -- ^ Label assigned based on a lemma label  
    | ByForm    -- ^ Based on labels of other forms within the same lexeme
    deriving (Eq, Ord, Show, Read)

instance Binary RelCode where
    put Exact   = put '1'
    put ByBase = put '2'
    put ByForm  = put '3'
    get = get >>= \x -> return $ case x of
        '1' -> Exact
        '2' -> ByBase
        '3' -> ByForm
        c   -> error $ "get: invalid RelCode code '" ++ [c] ++ "'"

-- | Origin of the form.
data Origin
    = Poli  -- ^ From polimorf
    | Dict  -- ^ From labeled dictionary
    | Both  -- ^ From both

-- | Merge the 'BaseMap' with the dictionary resource which maps forms to
-- sets of labels.  Every label is assigned a 'RelCode' which tells what
-- is the relation between the label and the form.  There are three
-- kinds of labels:
-- 'Exact' labels assigned in a direct manner, 'ByBase' labels assigned
-- to all forms which have a base form with a label in the input dictionary,
-- and 'ByForm' labels assigned to all forms which have a related form from the
-- same lexeme with a label in the input dictionary.
--
-- This function is far from being memory efficient right now.  If you plan to
-- run it with respect to the entire PoliMorf dictionary you should do it
-- on a machine with an abundance of available memory.
merge
    :: Ord a => BaseMap
    -> M.Map Form (S.Set a)
    -> M.Map Form (Origin, M.Map a RelCode)
merge poli dict0 = M.fromList
    [ (x, (origin x, combine x))
    | x <- keys ]
  where
    -- Keys in the output dictionary.
    poliKeys = M.keysSet poli
    dictKeys = M.keysSet dict0
    keys = S.toList (poliKeys `S.union` dictKeys)

    origin x
        | inPoli && inDict  = Both
        | inPoli            = Poli
        | otherwise         = Dict
      where
        inPoli = S.member x poliKeys
        inDict = S.member x dictKeys

    -- Combining function.
    combine x = (M.unionsWith min . catMaybes)
        [ label Exact  <$> M.lookup x dict0 
        , label ByBase <$> M.lookup x dict1
        , label ByForm <$> M.lookup x dict2 ]

    label :: Ord a => RelCode -> S.Set a -> M.Map a RelCode
    label code s = M.fromList [(x, code) | x <- S.toList s]

    -- Extended to all base forms of dict0 keys.
    dict1 = fromListWith mappend
        [ (lemma, x)
        | (_form, x) <- M.assocs dict0
        , lemma <- elemsOn poli _form ]

    -- Extended to all forms of dict0 keys.
    dict2 = fromListWith mappend
        [ (form', x)
        | (_form, x) <- M.assocs dict0
        , lemma <- elemsOn poli _form
        , form' <- elemsOn ilop lemma ]

    -- Inverse poli dictionary.
    ilop = fromListWith mappend
        [ (lemma, S.singleton _form)
        | (_form, lemmas) <- M.assocs poli
        , lemma <- S.toList lemmas ]

elemsOn :: (Ord a, Ord b) => M.Map a (S.Set b) -> a -> [b]
elemsOn m x = case x `M.lookup` m of
    Just s  -> S.toList s
    Nothing -> []

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> M.Map k a
fromListWith f xs =
    let update m (!k, !x) = M.insertWith' f k x m
    in  foldl' update M.empty xs
