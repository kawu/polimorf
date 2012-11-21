{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}

-- | The module provides functionality for manipulating PoliMorf, the
-- morphological dictionary for Polish. Apart from IO utilities there
-- is a 'merge' function which can be used to merge the PoliMorf with
-- another dictionary resources.

module Data.PoliMorf
( 
-- * Core types
  Form
, Base
, Tag
, Cat
, Entry (..)
, atomic

-- * Parsing
, readPoliMorf
, parsePoliMorf

-- * Merging
, Rule (..)
, apply
, toBase
, mkRuleMap
, BaseMap
, mkBaseMap
, FormMap
, mkFormMap
, RelCode (..)
, mergeWith
, merge
) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (catMaybes)
import Data.Monoid (mappend)
import Data.Binary (Binary, get, put)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.DAWG as D

import Debug.Trace (trace)

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

-- | Is the entry an atomic one?
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
    [_form, _base, _tag, _cat] -> Entry _form _base _tag _cat
    _   -> error $ "parsePoliRow: invalid row \"" ++ L.unpack row ++ "\""

-- | A rule for translating a form into other form (presumably lemma).
data Rule = Rule {
    -- | Number of characters to cut from the end of the form.
      cut       :: !Int
    -- | A suffix to paste.
    , suffix    :: !T.Text }
    deriving (Show, Eq, Ord)

-- | Apply the rule.
apply :: Rule -> T.Text -> T.Text
apply r x = T.take (T.length x - cut r) x `T.append` suffix r

-- | Determine the rule needed to translate the form to its base form.
toBase :: Entry -> Maybe Rule
toBase x
    | "sup" `T.isInfixOf` tag x && "naj" `T.isPrefixOf` form x = Nothing
    | "neg" `T.isInfixOf` tag x && "nie" `T.isPrefixOf` form x = Nothing
    | otherwise =
        let k = lcp (form x) (base x)
        in  Just $ Rule (T.length (form x) - k) (T.drop k (base x))
  where
    lcp a b = case T.commonPrefixes a b of
        Just (c, _, _)  -> T.length c
        Nothing         -> trace (show (form x, base x)) 0

-- | Make a rule to translate between two strings.
between :: T.Text -> T.Text -> Rule
between source dest =
    let k = lcp source dest
    in  Rule (T.length source - k) (T.drop k dest)
  where
    lcp a b = case T.commonPrefixes a b of
        Just (c, _, _)  -> T.length c
        Nothing         -> 0

-- | A map from forms to their possible base forms (there may be many since
-- the form may be a member of multiple lexemes).
type BaseMap = D.DAWG (S.Set Rule)

-- | A map from base forms to all their potential forms.
type FormMap = D.DAWG (S.Set Rule)

-- | Make the rule map from a list of entries.
mkRuleMap :: [(T.Text, T.Text)] -> D.DAWG (S.Set Rule)
mkRuleMap xs = D.fromListWith S.union $
    [ ( T.unpack x
      , S.singleton (between x y) )
    | (x, y) <- xs ]

-- | Make a DAWG from forms to their possible base forms (there may be many
-- since the form may be a member of multiple lexemes).
mkBaseMap :: [Entry] -> BaseMap
mkBaseMap = mkRuleMap . map ((,) <$> form <*> base)

-- | Make a DAWG from base forms to their potential forms.
mkFormMap :: [Entry] -> FormMap
mkFormMap = mkRuleMap . map ((,) <$> base <*> form)

-- | Reliability information: how did we assign a particular label to
-- a particular word form.
data RelCode
    = ByForm    -- ^ Based on labels of other forms within the same lexeme
    | ByBase    -- ^ Label assigned based on a lemma label  
    | Exact     -- ^ Label assigned in a direct manner
    deriving (Eq, Ord, Show, Read)

instance Binary RelCode where
    put Exact   = put '1'
    put ByBase  = put '2'
    put ByForm  = put '3'
    get = get >>= \x -> return $ case x of
        '1' -> Exact
        '2' -> ByBase
        '3' -> ByForm
        c   -> error $ "get: invalid RelCode code '" ++ [c] ++ "'"

-- | Merge the map from forms to their potential base forms with the dictionary
-- resource which maps forms to sets of labels.  Every label is assigned
-- a 'RelCode' which tells what is the relation between the label and the form.
-- It is a generalized version of the 'merge' function with additional
-- function @f x y y'label@ which can be used to determine the resultant
-- set of labels for the form @x@ given ,,similar'' form @y@ and its
-- original label @y'label@.  There are three kinds of labels:
-- 'Exact' labels assigned in a direct manner, 'ByBase' labels assigned
-- to all forms which have a base form with a label in the input dictionary,
-- and 'ByForm' labels assigned to all forms which have a related form from the
-- same lexeme with a label in the input dictionary.
mergeWith
    :: Ord a
    => (String -> String -> a -> a)
    -> BaseMap
    -> D.DAWG (S.Set a)
    -> D.DAWG (M.Map a RelCode)
mergeWith f poli dict0 = D.fromList
    [ (x, combine x)
    | x <- keys ]
  where
    -- Keys in the output dictionary.
    keys = join (D.keys poli) (D.keys dict0)

    -- Combining function.
    combine x = (M.unionsWith max . catMaybes)
        [ label Exact  <$> D.lookup x dict0 
        , label ByBase <$> D.lookup x dict1
        , label ByForm <$> D.lookup x dict2 ]

    label :: Ord a => RelCode -> S.Set a -> M.Map a RelCode
    label code s = M.fromList [(x, code) | x <- S.toList s]

    -- Extended to all base forms of dict0 keys.
    dict1 = D.fromListWith mappend
        [ (lemma, f'Set lemma _form x)
        | (_form, x) <- D.assocs dict0
        , lemma <- elemsOn poli _form ]

    -- Extended to all forms of dict0 keys.
    dict2 = D.fromListWith mappend
        [ (form', f'Set form' _form x)
        | (_form, x) <- D.assocs dict0
        , lemma <- elemsOn poli _form
        , form' <- elemsOn ilop lemma ]

    -- Inverse poli dictionary.
    ilop = mkRuleMap
        [ (base'Text, form'Text)
        | (form'String, rules) <- D.assocs poli
        , rule <- S.toList rules
        , let form'Text = T.pack form'String
        , let base'Text = apply rule form'Text ]
    
    -- Merge to ascending lists.
    join (x:xs) (y:ys)
        | x < y     = x : join xs (y:ys)
        | x > y     = y : join (x:xs) ys
        | otherwise = x : join xs ys
    join xs []  = xs
    join [] ys  = ys

    -- Version of f function working on label sets.
    f'Set v w = S.fromList . map (f v w) . S.toList

-- | A specialized version of the 'mergeWith' function which doesn't
-- change labels in the resultant 'D.DAWG'.
merge
    :: Ord a => BaseMap
    -> D.DAWG (S.Set a)
    -> D.DAWG (M.Map a RelCode)
merge = mergeWith $ \_ _ x -> x

elemsOn :: D.DAWG (S.Set Rule) -> String -> [String]
elemsOn m x = case x `D.lookup` m of
    Just s  ->
        [ T.unpack . apply rule . T.pack $ x
        | rule <- S.toList s ]
    Nothing -> []
