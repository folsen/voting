module Voting.View where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json

import Voting.Types exposing (..)
import Voting.Json exposing (..)

--------------------------
-- VIEW RENDERING STUFF --
--------------------------

-- | Top level view describing entire app
view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "vote-wrapper container" ]
    [ inputAreaView address model
    , itemListView address model
    ]

-- | Top part of the app with the input area
inputAreaView : Signal.Address Action -> Model -> Html
inputAreaView address model =
  header
    [ id "entry-header" ]
    [ h1 [] [ text "Address & Feature voting!" ]
    , div
        [ class "row" ]
        [ div
          [ class "col-sm-4"]
          [ select
              [ id "item-type"
              , class "form-control"
              , on "input" targetValue (Signal.message address << selectedItemType)
              ]
              [ option [] [ text <| toString Address ]
              , option [] [ text <| toString Feature ]
              ]
          ]
        , div
            [ class "col-sm-8" ]
            [ input
                [ id "new-vote"
                , class "form-control"
                , placeholder "What's needed?"
                , autofocus True
                , value model.field
                , name "newVote"
                , on "input" targetValue (Signal.message address << NewItem)
                , onEnter address CreateItem
                ]
                []
            ]
        ]
    ]

-- | View of the list of items, the items are sorted
--   by points and only the type selected in
--   `model.selectedItemType` are displayed
itemListView : Signal.Address Action -> Model -> Html
itemListView address model =
  section
    []
    [ ul
        [ class "vote-list" ]
        (itemsToDisplay address model)
    ]

-- | Filter, sort and render the item list
itemsToDisplay : Signal.Address Action -> Model -> List Html
itemsToDisplay address model =
  model.items
    |> List.filter (\i -> i.itemType == model.selectedItemType)
    |> List.sortBy (\i -> -i.points)
    |> List.map (renderItem address)


-- | Convert a string (gotten from some user input) to an action
--   to select an ItemType, if string can't be parsed into an ItemType
--   it will choose Address as default
selectedItemType : String -> Action
selectedItemType str =
  case strToItemType str of
    Ok itemType -> ItemTypeSelected itemType
    Err _       -> ItemTypeSelected Address

-- | Render a single item in the list
renderItem : Signal.Address Action -> Item -> Html
renderItem address item =
  li
    [ class "item" ]
    [ div [ class "left points" ] [ text (toString item.points) ]
    , div
        [ class "left arrows" ]
        [ div [ class "arrow-up", onClick address (Upvote item.uid) ] []
        , div [ class "arrow-down", onClick address (Downvote item.uid) ] []
        ]
    , div [ class "left desc" ] [ text item.description ]
    ]

-- | A function that will send a message to an address when
--   the enter key is pressed
onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)

-- | Function to check if a keypress is enter
is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"
