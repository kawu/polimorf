{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.LMF.Hist
( Entry (..)
, parseHist
) where

import qualified Data.Text as T
import Text.XML.PolySoup

data Entry = Entry
    { lxId  :: T.Text        -- ^ Lexical entry identifier
    , forms :: [T.Text] }    -- ^ Entry forms
    deriving (Show, Read, Eq, Ord)

lmfP :: XmlParser String [Entry]
lmfP = true //> lexEntryP

lexEntryP :: XmlParser String Entry
lexEntryP = tag "LexicalEntry" *> getAttr "id" >^> \lexId -> do
    many_ (cut $ tag "feat")
    forms  <- concat <$> many formP
    Entry (T.pack lexId) (map T.pack forms) <$ ignore

formP :: XmlParser String [String]
formP = tag "Lemma" <|> tag "WordForm" //> featP "writtenForm"

featP :: String -> XmlParser String String
featP att = cut (tag "feat" *> hasAttr "att" att *> getAttr "val")

parseHist :: String -> [Entry]
parseHist = parseXml lmfP
