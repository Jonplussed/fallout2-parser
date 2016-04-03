{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import Data.Attoparsec.Text (Parser, parseOnly)

import Fallout2.Parser.Internal
import Fallout2.Types
import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "keyVal" $ do

    it "parses a key/value pair" $ do
      let str = "key:value"
          res = ("key","value")
      testParser keyVal str res

    it "parses a key-only pair" $ do
      let str = "key"
          res = ("key","")
      testParser keyVal str res

    it "parses many keyVals when comma-separated" $ do
      let str = "foo, bar:baz, quux:norf"
          res = [("foo",""), ("bar","baz"), ("quux","norf")]
      testParser (commaSep keyVal) str res

  describe "parseNpcLine" $ do

    it "parses an NPC line" $ do
      let str =
            "type_01=ratio:60%, pid:16777418, Item:7(wielded), Item:(0-10)41, \
            \Script:484    ; Male Hunter w/ Spear"
          res = Npc
            { npcName = Name "Male Hunter w/ Spear"
            , npcStatus = Ratio 60
            , npcPid = PID 16777418
            , npcItems =
                [ Item (ItemID 41) (Inventory 0 10)
                , Item (ItemID 7) Wielded
                ]
            }
      testParser parseNpcLine str res

  describe "parseComment" $ do

    it "parses a comment" $ do
      let str = ";  test comment"
          res = "test comment"
      testParser comment str res

  describe "parseItem" $ do

    it "parses a wielded item" $ do
      let str = "7(wielded)"
          res = Item (ItemID 7) Wielded
      testParser parseItem str res

    it "parses an inventory item" $ do
      let str = "(1-4)29"
          res = Item (ItemID 29) (Inventory 1 4)
      testParser parseItem str res

  describe "parseRatio" $ do

    it "parses a ratio value" $ do
      let str = "50%"
          res = Ratio 50
      testParser parseRatio str res

-- utils

parse :: Monad m => Parser a -> Text -> m a
parse parser = either fail return . parseOnly parser

testParser :: (Eq a, Show a) => Parser a -> Text -> a -> Expectation
testParser parser text expected = do
    result <- parser `parse` text
    result `shouldBe` expected
