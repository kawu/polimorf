{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}

module Data.PoliMorf
( PoliMorf
, joinDict
, RelCode (..)
) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Monoid (Monoid, mappend)
import Data.List (foldl')
import Data.Maybe (maybeToList)

import Data.Binary (Binary, get, put)

type Form  = T.Text
type Lemma = T.Text
type PoliMorf = M.Map Form [Lemma]

-- | Reliability information: how did we assign a particular label to
-- a particular word form.
data RelCode
    = Exact
    | ByLemma   -- ^ Label assigned based on lemma label  
    | ByForm    -- ^ Based on labels of other forms within the same lexeme.
    deriving (Eq, Ord, Show, Read)

instance Binary RelCode where
    put Exact   = put '1'
    put ByLemma = put '2'
    put ByForm  = put '3'
    get = get >>= \x -> return $ case x of
        '1' -> Exact
        '2' -> ByLemma
        '3' -> ByForm

joinDict :: Monoid m => PoliMorf -> M.Map Form m
         -> M.Map Form (Maybe (m, RelCode))
joinDict poli dict0 =
    combine poli dict0 f
  where
    f x | Just y <- M.lookup x dict0 = Just (y, Exact)
        | Just y <- M.lookup x dict1 = Just (y, ByLemma)
        | Just y <- M.lookup x dict2 = Just (y, ByForm)
        | otherwise = Nothing

    -- | Extended to all base forms of dict0 keys.
    dict1 = fromListWith mappend
        [ (lemma, x)
        | (form, x) <- M.assocs dict0
        , lemmas <- maybeToList (form `M.lookup` poli)
        , lemma <- lemmas ]

    -- | Extended to all forms of dict0 keys
    dict2 = fromListWith mappend
        [ (form', x)
        | (form, x) <- M.assocs dict0
        , lemmas <- maybeToList (form `M.lookup` poli)
        , lemma  <- lemmas
        , forms' <- maybeToList (lemma `M.lookup` ilop)
        , form'  <- forms' ]

    -- | Inverse poli dictionary.
    ilop = fmap S.toList $ fromListWith mappend
        [ (lemma, S.singleton form)
        | (form, lemmas) <- M.assocs poli
        , lemma <- lemmas ]

combine :: Ord a => M.Map a b -> M.Map a c -> (a -> d) -> M.Map a d
combine m n f =
    let keys = S.toList (M.keysSet m `S.union` M.keysSet n)
    in  M.fromList [(x, f x) | x <- keys]

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> M.Map k a
fromListWith f xs =
    let update m (!k, !x) = M.insertWith' f k x m
    in  foldl' update M.empty xs

-- joinHist :: Ord a => PoliMorf -> M.Map Form [a]
--          -> M.Map Form (Maybe ([a], RelCode))
-- joinHist poli hist =
--     combine poli hist f
--   where
--     f x | Just y <- M.lookup x hist
--             = Just (y, Exact)
--         | Just y <- M.lookup x poli >>= flip M.lookup hist
--             = Just (y, ByLemma)
--         | Just y <- M.lookup x poli >>= flip M.lookup hist'
--             = Just (y, ByForm)
--         | otherwise = Nothing
--     hist' = M.fromList
--         [ (lemma, y)
--         | (form, lemma) <- M.assocs poli
--         , y <- maybeToList (form `M.lookup` hist) ]
