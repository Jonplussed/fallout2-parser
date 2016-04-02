{-# LANGUAGE OverloadedStrings #-}

module Fallout2.Parser.Tokens where

import Data.Char (Char, isSpace)
import Data.Text (Text)

-- chars

colon_ =        ':'               :: Char
comma_ =        ';'               :: Char
equalSign_ =    '='               :: Char
lbracket_ =     '['               :: Char
rbracket_ =     ']'               :: Char
semicolon_ =    ';'               :: Char
underscore_ =   '_'               :: Char

-- strings

type_ =         "type"            :: Text
encounter_ =    "Encounter"       :: Text
dead_ =         "Dead"            :: Text
position_ =     "position"        :: Text
surrounding_ =  "Surrounding"     :: Text
distance_ =     "Distance"        :: Text
ratio_ =        "Ratio"           :: Text
item_ =         "Item"            :: Text
script_ =       "Script"          :: Text
pid_ =          "pid"             :: Text
straightLine_ = "straight_line"   :: Text
spacing_ =      "spacing"         :: Text
cone_ =         "cone"            :: Text
huddle_ =       "huddle"          :: Text
doubleLine_ =   "double_line"     :: Text
wedge_ =        "wedge"           :: Text

-- tests

isNotComma =    (/= comma_)
isNotSpace =    not . isSpace
