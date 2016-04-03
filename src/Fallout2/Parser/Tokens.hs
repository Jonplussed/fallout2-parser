{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Fallout2.Parser.Tokens where

import Data.Text (Text)

-- chars

colon_ =        ':'               :: Char
comma_ =        ','               :: Char
equals_ =       '='               :: Char
lbracket_ =     '['               :: Char
rbracket_ =     ']'               :: Char
lparens_ =      '('               :: Char
rparens_ =      ')'               :: Char
semicolon_ =    ';'               :: Char
underscore_ =   '_'               :: Char
percent_ =      '%'               :: Char
minus_ =        '-'               :: Char

-- strings

type_ =         "type"            :: Text
encounter_ =    "Encounter"       :: Text
dead_ =         "Dead"            :: Text
position_ =     "position"        :: Text
surrounding_ =  "Surrounding"     :: Text
distance_ =     "Distance"        :: Text
ratio_ =        "ratio"           :: Text
item_ =         "Item"            :: Text
script_ =       "Script"          :: Text
pid_ =          "pid"             :: Text
straightLine_ = "straight_line"   :: Text
spacing_ =      "spacing"         :: Text
cone_ =         "cone"            :: Text
huddle_ =       "huddle"          :: Text
doubleLine_ =   "double_line"     :: Text
wedge_ =        "wedge"           :: Text
wielded_ =      "wielded"         :: Text
