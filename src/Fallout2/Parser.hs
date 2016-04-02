module Fallout2.Parser where

import Prelude hiding (takeWhile)

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Fallout2.Types as F
import qualified Fallout2.Types as F

import Data.Attoparsec.Text
import Fallout2.Parser.Tokens
import Fallout2.Parser.Permutation

comment :: Parser T.Text
comment = do
    char semicolon_
    takeTill isEndOfLine

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

npcType :: Parser ()
npcType = do
    string type_
    char underscore_
    digit
    digit
    return ()

mapKeyVals :: [(T.Text, T.Text)] -> KeyValsMap
mapKeyVals = L.foldl' (\kvs (k,v) -> M.insertWith (++) k [v] kvs) M.empty

npcLine :: Parser F.Npc
npcLine = do
    npcType
    char equalSign_
    keyVals <- mapKeyVals <$> commaSep keyVal
    skipSpace
    name <- F.Name <$> comment
    runPermParser (makeNpc name) keyVals

parseDead :: Parser F.NpcStatus
parseDead = undefined

parseRatio :: Parser F.NpcStatus
parseRatio = undefined

parsePid :: Parser F.PID
parsePid = undefined

parseItem :: Parser F.Item
parseItem = undefined

makeNpc :: F.Name -> PermParser F.Npc
makeNpc name =
    F.Npc name
      <$> req1Of [(dead_, parseDead), (ratio_, parseRatio)]
      <*> req1 pid_ parsePid
      <*> optN item_ parseItem
