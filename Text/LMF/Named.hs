{-# LANGUAGE BangPatterns #-}

module Text.LMF.Named
( Entry (..)
, parseNamed
) where

import qualified Data.Text as T
import Text.XML.PolySoup

data Entry = Entry
    { neOrth :: T.Text
    , neType :: T.Text }
    deriving (Show, Read, Eq, Ord)

lmfP :: XmlParser String [Entry]
lmfP = true ##> lexEntryP

lexEntryP :: XmlParser String [Entry]
lexEntryP = tag "LexicalEntry" `joinR` do
    many_ $ cut $ tag "feat"
    words <- many wordP
    !sense <- senseP
    return [Entry x sense | !x <- words]

wordP :: XmlParser String T.Text
wordP = head <$> (tag "Lemma" <|> tag "WordForm" /> featP "writtenForm")

senseP :: XmlParser String T.Text
senseP = head <$> (tag "Sense" //> featP "externalReference" <|> featP "label")

featP :: String -> XmlParser String T.Text
featP att = T.pack <$> cut (tag "feat" *> hasAttr "att" att *> getAttr "val")

parseNamed :: String -> [Entry]
parseNamed = parseXML lmfP
