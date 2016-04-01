{-# LANGUAGE OverloadedStrings #-}

module Fallout2.Parser.Tokens
  ( colon_
  , comma_
  , equalSign_
  , lbracket_
  , rbracket_
  , semicolon_
  , underscore_

  , type_
  , encounter_
  , dead_
  , position_
  , surrounding_
  , distance_
  , ratio_
  , item_
  , script_
  , pid_
  , straightLine_
  , spacing_
  , cone_
  , huddle_
  , doubleLine_
  , wedge_

  , isNotComma
  , isNotSpace
  ) where

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
