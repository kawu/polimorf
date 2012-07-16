{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}

module Data.PoliMorf
( PoliMorf
, combine
, joinNamed
, joinHist
, RelCode (..)
) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Data.List (foldl')
import Data.Maybe (maybeToList)

import Data.Binary (Binary, get, put)

type Form  = T.Text
type Lemma = T.Text
type PoliMorf = M.Map Form Lemma

combine :: Ord a => M.Map a b -> M.Map a c -> (a -> d) -> M.Map a d
combine m n f =
    let keys = S.toList (M.keysSet m `S.union` M.keysSet n)
    in  M.fromList [(x, f x) | x <- keys]

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

joinNamed :: Ord a => PoliMorf -> M.Map Form [a]
          -> M.Map Form (Maybe ([a], RelCode))
joinNamed poli named =
    combine poli named f
  where
    f x | Just ys <- M.lookup x named
            = Just (ys, Exact)
        | Just ys <- M.lookup x poli >>= flip M.lookup named
            = Just (ys, ByLemma)
        | Just ys <- M.lookup x poli >>= flip M.lookup named'
            = Just (ys, ByForm)
        | otherwise = Nothing
    named' = fmap S.toList $ fromListWith S.union
        [ (lemma, S.fromList ys)
        | (form, lemma) <- M.assocs poli
        , ys <- maybeToList (form `M.lookup` named) ]

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> M.Map k a
fromListWith f xs =
    let update m (!k, !x) = M.insertWith' f k x m
    in  foldl' update M.empty xs

joinHist :: Ord a => PoliMorf -> M.Map Form a
         -> M.Map Form (Maybe (a, RelCode))
joinHist poli hist =
    combine poli hist f
  where
    f x | Just y <- M.lookup x hist
            = Just (y, Exact)
        | Just y <- M.lookup x poli >>= flip M.lookup hist
            = Just (y, ByLemma)
        | Just y <- M.lookup x poli >>= flip M.lookup hist'
            = Just (y, ByForm)
        | otherwise = Nothing
    hist' = M.fromList
        [ (lemma, y)
        | (form, lemma) <- M.assocs poli
        , y <- maybeToList (form `M.lookup` hist) ]
