{-# LANGUAGE
    DeriveGeneric
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  #-}

module Fallout2.Types
  ( Condition (..)
  , Distance (..)
  , ItemID (..)
  , Name (..)
  , PID (..)
  , Script (..)
  , Spacing (..)
  , ItemStatus (..)
  , NpcType (..)
  , NpcPosition (..)
  , Item (..)
  , Npc (..)
  , Encounter (..)
  ) where

import Data.Aeson ((.=))
import Data.Char (isLower, toLower)
import Data.Monoid ((<>))
import GHC.Generics (Generic (..))

import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

newtype Condition = Condition T.Text deriving (Eq, Show, A.ToJSON)
newtype Distance = Distance Int deriving (Eq, Show, A.ToJSON)
newtype ItemID = ItemID Int deriving (Eq, Show, A.ToJSON)
newtype Name = Name T.Text deriving (Eq, Show, A.ToJSON)
newtype PID = PID Int deriving (Eq, Show, A.ToJSON)
newtype Script = Script Int deriving (Eq, Show, A.ToJSON)
newtype Spacing = Spacing Int deriving (Eq, Show, A.ToJSON)

data ItemStatus
  = Wielded
  | Carried Int Int
  deriving (Eq, Show)

instance A.ToJSON ItemStatus where
  toJSON Wielded =
      A.String "wielded"
  toJSON (Carried low high) =
      A.object
        [ "carried_min" .= A.toJSON low
        , "carried_max" .= A.toJSON high
        ]

data NpcType
  = Dead
  | Ratio Int
  deriving (Eq, Show)

instance A.ToJSON NpcType where
  toJSON Dead =
    A.String "dead"
  toJSON (Ratio r) =
    A.object
      [ "ratio" .= A.toJSON (show r <> "%")
      ]

data NpcPosition
  = Surrounding
  | StraightLine
  | DoubleLine
  | Wedge
  | Cone
  | Huddle
  deriving (Eq, Show)

instance A.ToJSON NpcPosition where
  toJSON pos =
      A.String $ case pos of
        Surrounding -> "surrounding"
        StraightLine -> "straight_line"
        DoubleLine -> "double_line"
        Wedge -> "wedge"
        Cone -> "cone"
        Huddle -> "huddle"

data Item = Item
  { itemId :: ItemID
  , itemStatus :: ItemStatus
  } deriving (Eq, Generic, Show)

instance A.ToJSON Item where
  toJSON = dropPrefixToJSON

data Npc = Npc
  { npcName :: Name
  , npcType :: NpcType
  , npcPid :: PID
  , npcItems :: [Item]
  , npcDistance :: Distance
  , npcCondition :: Condition
  } deriving (Eq, Generic, Show)

instance A.ToJSON Npc where
  toJSON = dropPrefixToJSON

data Encounter = Encounter
  { encName :: Name
  , encNpcs :: [Npc]
  , encPosition :: NpcPosition
  , encSpacing :: Spacing
  } deriving (Eq, Generic, Show)

instance A.ToJSON Encounter where
  toJSON = dropPrefixToJSON

-- internal

dropPrefixOptions :: A.Options
dropPrefixOptions =
    A.defaultOptions
      { A.fieldLabelModifier = same . downcase . dropPrefix
      }
  where
    same :: String -> String
    same = A.fieldLabelModifier A.defaultOptions

    downcase :: String -> String
    downcase (x:xs) = toLower x : xs
    downcase [] = []

    dropPrefix :: String -> String
    dropPrefix = dropWhile isLower

dropPrefixToJSON :: (A.GToJSON (Rep a), Generic a) => a -> A.Value
dropPrefixToJSON = A.genericToJSON dropPrefixOptions
