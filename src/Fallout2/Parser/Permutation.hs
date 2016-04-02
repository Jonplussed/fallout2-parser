{-# LANGUAGE OverloadedStrings #-}

module Fallout2.Parser.Permutation
  ( KeyValsMap
  , opt1
  , req1
  , optN
  , reqN
  , opt1Of
  , req1Of
  ) where

import Control.Monad.Reader (Reader, ask)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Attoparsec.Text (Parser, parseOnly)

import qualified Data.Text as T
import qualified Data.Map.Strict as M

type KeyValsMap = M.Map T.Text [T.Text]
type PermParser = ExceptT String (Reader KeyValsMap)

opt1 :: T.Text -> Parser a -> PermParser (Maybe a)
opt1 key parser = do
    textResult <- M.lookup key <$> ask
    case textResult of
      Just [text] -> Just <$> parseE parser text
      Just _ -> throwE $ onlyOneAllowed key
      Nothing -> return Nothing

req1 :: T.Text -> Parser a -> PermParser a
req1 key parser = do
    optResult <- opt1 key parser
    case optResult of
      Just val -> return val
      Nothing -> throwE $ missingReqKey key

optN :: T.Text -> Parser a -> PermParser [a]
optN key parser = do
    textResult <- M.lookup key <$> ask
    case textResult of
      Just texts -> sequence $ map (parseE parser) texts
      Nothing -> return []

reqN :: T.Text -> Parser a -> PermParser [a]
reqN key parser = do
    optResult <- optN key parser
    if null optResult
      then throwE $ atLeastOneReq key
      else return optResult

opt1Of :: [(T.Text, Parser a)] -> PermParser (Maybe a)
opt1Of opts = do
    optValues <- sequence $ map (\(k,p) -> opt1 k p) opts
    case catMaybes optValues of
      [] -> return Nothing
      [val] -> return $ Just val
      _ -> throwE . onlyOneOfKeyAllowed $ map fst opts

req1Of :: [(T.Text, Parser a)] -> PermParser a
req1Of opts = do
    optResults <- opt1Of opts
    case optResults of
      Nothing -> throwE . oneOfKeyReq $ map fst opts
      Just val -> return val

-- error messages

invalidValue :: T.Text -> String -> String
invalidValue value err =
    "error parsing value \"" ++ T.unpack value ++ "\": " ++ err

onlyOneAllowed :: T.Text -> String
onlyOneAllowed key = T.unpack $
    "only one value allowed for key: " <> key

missingReqKey :: T.Text -> String
missingReqKey key = T.unpack $
    "missing the required key: " <> key

atLeastOneReq :: T.Text -> String
atLeastOneReq key = T.unpack $
    "at least one value required for key: " <> key

onlyOneOfKeyAllowed :: [T.Text] -> String
onlyOneOfKeyAllowed keys = T.unpack $
    "only one key allowed of: " <> T.intercalate ", " keys

oneOfKeyReq :: [T.Text] -> String
oneOfKeyReq keys = T.unpack $
    "one key required of: " <> T.intercalate ", " keys

-- internal

parseE :: Parser a -> T.Text -> PermParser a
parseE parser text =
    either (throwE . invalidValue text) return $ parseOnly parser text
