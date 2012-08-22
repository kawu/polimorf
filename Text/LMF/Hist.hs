{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.LMF.Hist
( Entry (..)
, parseHist
) where

import qualified Data.Text as T
import Text.XML.PolySoup

data Entry = Entry
    { lxId :: T.Text    -- ^ Lexical entry identifier
    , base :: T.Text }  -- ^ Base form
    deriving (Show, Read, Eq, Ord)

lmfP :: XmlParser String [Entry]
lmfP = true //> lexEntryP

lexEntryP :: XmlParser String Entry
lexEntryP = (tag "LexicalEntry" *> getAttr "id") `join` \lexId -> do
    many_ $ cut $ tag "feat"
    -- xs <- many wordP
    lemma <- lemmaP <* ignore
    return (Entry (T.pack lexId) lemma)

lemmaP :: XmlParser String T.Text
lemmaP = getIt <$> (tag "Lemma" //> featP "writtenForm")
  where  getIt []     = ""  -- ^ Abnormal, lemma should be always present!
         getIt (x:xs) = T.pack x

-- wordP :: XmlParser String String
-- wordP = head <$> (tag "Lemma" <|> tag "WordForm" //> featP "writtenForm")

featP :: String -> XmlParser String String
featP att = cut (tag "feat" *> hasAttr "att" att *> getAttr "val")

parseHist :: String -> [Entry]
parseHist = parseXml lmfP
