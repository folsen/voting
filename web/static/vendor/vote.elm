import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects, Never)
import Json.Decode as Json exposing ((:=))
import Json.Encode as Encode
import StartApp
import String
import Task exposing (..)
import Debug

-----------
-- TYPES --
-----------

-- | Fundamental model of the world
type alias Model =
  { items : List Item
  , field : String
  , selectedItemType : ItemType
  , uid : Int
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
  | Upvote ItemID
  | Downvote ItemID
  | ItemTypeSelected ItemType
  | UpdateItems (Maybe (List Item))

-------------------------------------
-- FUNCTIONS TO UPDATE WORLD STATE --
-------------------------------------

-- | Top level update function describing how to go from
--   one state to the next when taking an action
update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)

    NewItem str -> ({ model |
                       field <- str
                    }, Effects.none)

    CreateItem ->
      let newItem = Item model.uid model.field model.selectedItemType 0
      in ({ model |
             uid <- model.uid + 1,
             field <- "",
             items <-
               if String.isEmpty model.field
                  then model.items
                  else model.items ++ [newItem]
          }, apiPostItem newItem)

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
      in ({ model |
              uid   <- Maybe.withDefault model.uid <| List.maximum <| List.map (\i -> i.uid) allItems,
              items <- allItems }
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


-------------------------------
-- ENCODING / DECODING STUFF --
-------------------------------

-- | Make an encoder slightly easier to read
type alias Encoder a = a -> Encode.Value

-- | Convert string to ItemType
decodeItemType : Json.Decoder ItemType
decodeItemType =
  Json.customDecoder Json.string strToItemType

-- | Encode an Item as JSON
encodeItem : Encoder Item
encodeItem item =
  Encode.object
    [ ("item", Encode.object
                [ ("description", Encode.string item.description)
                , ("item_type"  , Encode.string <| toString item.itemType)
                , ("points"     , Encode.int item.points)
                ])
    ]

-- | Decode a JSON object to an Item
decodeItem : Json.Decoder Item
decodeItem =
  Json.object4 Item
    ("id"          := Json.int)
    ("description" := Json.string)
    ("item_type"   := decodeItemType)
    ("points"      := Json.int)

-- | Convert something that can be encoded to JSON into an HTTP JSON Body
toJsonBody : Encoder a -> a -> Http.Body
toJsonBody encoder x = Http.string <| Encode.encode 0 (encoder x)

-- | Convert a list of JSON objects (under the key `data`) to a list of Items
itemList : Json.Decoder (List Item)
itemList = Json.at ["data"]
            (Json.list decodeItem)

-- | Convert a string to an ItemType or return an error
strToItemType : String -> Result String ItemType
strToItemType str =
  case str of
    "Feature" -> Ok Feature
    "Address" -> Ok Address
    x         -> Err ("Unknown item type: " ++ x)

---------------
-- API STUFF --
---------------

-- | Get all items
apiGetItems : Effects Action
apiGetItems =
  let url = "/api/items"
  in Http.get itemList url
       |> Task.toMaybe
       |> Task.map UpdateItems
       |> Effects.task

-- | Post a single Item (creating a new item)
apiPostItem : Item -> Effects Action
apiPostItem item =
  let url = "/api/items"
  in postJson decodeItem url (toJsonBody encodeItem item)
       |> Task.toMaybe
       |> Task.map (\_ -> NoOp)
       |> Effects.task

-- | Put an item (updating it)
apiPutItem : Item -> Effects Action
apiPutItem item =
  let url = "/api/items/" ++ toString item.uid
  in putJson decodeItem url (toJsonBody encodeItem item)
       |> Task.toMaybe
       |> Task.map (\_ -> NoOp)
       |> Effects.task

-- | Generic function to POST `application/json` data
postJson : Json.Decoder value -> String -> Http.Body -> Task Http.Error value
postJson decoder url body =
  Http.fromJson decoder
  <| Http.send Http.defaultSettings
      { verb = "POST"
      , headers = [("Content-Type", "application/json")]
      , url = url
      , body = body
      }

-- | Generic function to PUT `application/json` data
putJson : Json.Decoder value -> String -> Http.Body -> Task Http.Error value
putJson decoder url body =
  Http.fromJson decoder
  <| Http.send Http.defaultSettings
      { verb = "PUT"
      , headers = [("Content-Type", "application/json")]
      , url = url
      , body = body
      }

--------------
-- PLUMBING --
--------------

-- | Create a default model, used as the world state when the application
--   starts, also declare which effects to execute on startup
initModel : (Model, Effects Action)
initModel =
  ( { items = []
    , field = ""
    , selectedItemType = Address
    , uid   = 0
    }
  , apiGetItems
  )


-- | port is an elm architecture thingamajig to connect together
--   tasks and effects
port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

-- | The top level StartApp app
app : StartApp.App Model
app =
  StartApp.start
    { init = initModel
    , update = update
    , view = view
    , inputs = []
    }

-- | Main function that gets run when the application runs
main : Signal Html
main = app.html
