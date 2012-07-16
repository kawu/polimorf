{-# LANGUAGE BangPatterns #-}

module Text.Prolexbase
( Entry (..)
, parseProlexbase
) where

import qualified Data.Text as T
import Data.List.Split (wordsBy)

data Entry = Entry
    { neOrth :: T.Text
    , neType :: T.Text }

parseProlexbase :: String -> [Entry]
parseProlexbase = map parseWikiRow . lines

-- parseWikiRow :: String -> [(NE, Type)]
-- parseWikiRow row =
--     let xs = groupBy' 4 $ split (=='\t') row
--         (body, desc) = (init xs, last xs)
--         label = desc !! 2
--         parseLang body =
--             let x = T.pack (body !! 0)
--                 y = T.pack (body !! 1 ++ label)
--             in  x `seq` y `seq` (x, y)
--     in  map parseLang body
--   where
--     groupBy' k [] = []
--     groupBy' k xs = take k xs : groupBy' k (drop k xs)

parseWikiRow :: String -> Entry
parseWikiRow row =
    let xs = wordsBy (=='\t') row
        body = head $ groupBy' 4 $ xs
        desc = lastk 4 xs
        label = desc !! 2
        parseLang body =
            let x = T.pack (body !! 0)
                y = T.pack (body !! 1 ++ label)
            in  x `seq` y `seq` Entry x y
    in  parseLang body
  where
    lastk k xs = drop (length xs - k) xs
    groupBy' k [] = []
    groupBy' k xs = take k xs : groupBy' k (drop k xs)
