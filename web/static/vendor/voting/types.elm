module Voting.Types where

-----------
-- TYPES --
-----------

-- | Fundamental model of the world
type alias Model =
  { items : List Item
  , field : String
  , selectedItemType : ItemType
  }

emptyModel : Model
emptyModel =
  { items = []
  , field = ""
  , selectedItemType = Address
  }

-- | Convenience type to make things more clear
type alias ItemID = Int

-- | An item, uid is used as identifier so should be unique
type alias Item =
  { uid : ItemID
  , description : String
  , itemType : ItemType
  , points : Int
  }

-- | An item can have a type
type ItemType
  = Address
  | Feature

-- | The types of actions we can take in our application
type Action
  = NoOp
  | NewItem String
  | CreateItem
  | InsertItem (Maybe Item)
  | Upvote ItemID
  | Downvote ItemID
  | ItemTypeSelected ItemType
  | UpdateItems (Maybe (List Item))
