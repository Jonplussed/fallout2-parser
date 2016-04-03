module Fallout2.Parser where

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

npcType :: Parser ()
npcType = do
    string type_
    char underscore_
    skipWhile C.isDigit
    return ()

npcLine :: Parser F.Npc
npcLine = do
    npcType
    char equals_
    keyVals <- fromList <$> commaSep keyVal
    skipSpace
    name <- F.Name <$> comment
    runKeyValParser (makeNpc name) keyVals

parseDead :: Parser F.NpcStatus
parseDead = return F.Dead

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
    defaultRange = F.Carried 1 1

    parseRange :: Parser F.ItemStatus
    parseRange = do
        char lparens_
        minHeld <- decimal
        char minus_
        maxHeld <- decimal
        char rparens_
        return $ F.Carried minHeld maxHeld

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
commaSep = flip sepBy (char comma_ >> skipSpace)

keyVal :: Parser (T.Text, T.Text)
keyVal = do
    phrase <- takeWhile isSamePhrase
    let (key, val) = T.breakOn (T.singleton colon_) phrase
    return (key, removeColon val)
  where
    isSamePhrase c = isNotComma c && isNotSpace c
    removeColon = T.drop 1

-- utils

char :: C.Char -> Parser ()
char = void . A.char

string :: T.Text -> Parser ()
string = void . A.string
