module Fallout2.Parser.Internal where

import Control.Applicative ((<|>))
import Control.Monad (void)

import qualified Data.Attoparsec.Text as A
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Fallout2.Types as F

import Data.Attoparsec.Text hiding (char, string)
import Fallout2.Parser.Tokens
import Fallout2.Parser.KeyVal
import Prelude hiding (takeWhile)

-- parsing an NPC line (e.g. type_01=ratio:25%,Item...)

ignoreNpcType :: Parser ()
ignoreNpcType = do
    string type_
    char underscore_
    skipWhile C.isDigit
    return ()

parseNpcLine :: Parser F.Npc
parseNpcLine = do
    ignoreNpcType
    char equals_
    keyVals <- fromList <$> commaSep keyVal
    skipSpace
    name <- F.Name <$> comment
    runKeyValParser (makeNpc name) keyVals

parseDead :: Parser F.NpcStatus
parseDead = return F.Dead <* string dead_

parseRatio :: Parser F.NpcStatus
parseRatio = do
    ratio <- decimal
    char percent_
    return $ F.Ratio ratio

parsePid :: Parser F.PID
parsePid = F.PID <$> decimal

parseItem :: Parser F.Item
parseItem = do
    range <- option defaultRange parseRange
    itemId <- decimal
    status <- option range parseWielded
    return $ F.Item (F.ItemID itemId) status
  where
    defaultRange :: F.ItemStatus
    defaultRange = F.Inventory 1 1

    parseRange :: Parser F.ItemStatus
    parseRange = do
        char lparens_
        minHeld <- decimal
        char minus_
        maxHeld <- decimal
        char rparens_
        return $ F.Inventory minHeld maxHeld

    parseWielded :: Parser F.ItemStatus
    parseWielded = do
        char lparens_
        string wielded_
        char rparens_
        return F.Wielded

makeNpc :: F.Name -> PermParser F.Npc
makeNpc name =
    F.Npc name
      <$> req1Of [(dead_, parseDead), (ratio_, parseRatio)]
      <*> req1 pid_ parsePid
      <*> optN item_ parseItem

-- general syntax

comment :: Parser T.Text
comment = do
    char semicolon_
    T.strip <$> takeTill isEndOfLine

commaSep :: Parser a -> Parser [a]
commaSep = flip sepBy (char comma_ >> space)

keyVal :: Parser (T.Text, T.Text)
keyVal = keyValue <|> keyOnly
  where
    isValueEnd :: Char -> Bool
    isValueEnd c = c == comma_ || C.isSpace c || isEndOfLine c

    isKeyEnd :: Char -> Bool
    isKeyEnd c = c == colon_ || isValueEnd c

    keyValue :: Parser (T.Text, T.Text)
    keyValue = do
        key <- takeTill isKeyEnd
        char colon_
        value <- takeTill isValueEnd
        return (key, value)

    keyOnly :: Parser (T.Text, T.Text)
    keyOnly = do
        key <- takeTill isValueEnd
        return (key, mempty)

-- overrides

char :: C.Char -> Parser ()
char = void . A.char

string :: T.Text -> Parser ()
string = void . A.string
