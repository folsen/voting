module Voting.Action where

import Effects exposing (Effects, Never)
import String

import Voting.Types exposing (..)
import Voting.Api exposing (..)

-------------------------------------
-- FUNCTIONS TO UPDATE WORLD STATE --
-------------------------------------

-- | Top level update function describing how to go from
--   one state to the next when taking an action
update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)

    NewItem str ->
      ( { model | field <- str }
      , Effects.none )

    CreateItem ->
      let newItem =
            { uid         = 0
            , description = model.field
            , itemType    = model.selectedItemType
            , points      = 0
            }
      in ( { model | field <- "" }
         , apiPostItem newItem )

    InsertItem item ->
      let newItem = Maybe.withDefault
                      (Item 0 "Error creating item" model.selectedItemType 0)
                      item
      in ( { model | items <- model.items ++ [newItem] }
         , Effects.none )

    Upvote id ->
      let (newItems, effect) = upvote id model.items
      in ({ model | items <- newItems }, effect)

    Downvote id ->
      let (newItems, effect) = downvote id model.items
      in ({ model | items <- newItems }, effect)

    ItemTypeSelected itemType ->
      ({ model | selectedItemType <- itemType }
      , Effects.none)

    UpdateItems items ->
      let allItems = Maybe.withDefault [Item 0 "can't parse server response" Address 0] items
      in ({ model | items <- allItems }
         , Effects.none)


-- | Upvote a single item in a list of items
upvote : ItemID -> List Item -> (List Item, Effects Action)
upvote id items =
  let upd i = { i | points <- i.points + 1 }
  in ( updateItems id items upd
     , updateItem id items upd)

-- | Downvote a single item in a list of items
downvote : ItemID -> List Item -> (List Item, Effects Action)
downvote id items =
  let upd i = { i | points <- i.points - 1 }
  in ( updateItems id items upd
     , updateItem id items upd)

-- | An effect to push the new item to the backend
updateItem : ItemID -> List Item -> (Item -> Item) -> Effects Action
updateItem id items f =
  case find (\i -> i.uid == id) items of
    Nothing -> Effects.none
    Just i  -> apiPutItem <| f i

-- | Helper function to update a single item in a list of items
updateItems : ItemID -> List Item -> (Item -> Item) -> List Item
updateItems id items f =
  let upd i = if i.uid == id then f i else i
  in List.map upd items

-- | List helper function to find an item
find : (a -> Bool) -> List a -> Maybe a
find f xs =
  let first = List.take 1 <| List.filter f xs
  in case first of
      []  -> Nothing
      [x] -> Just x
