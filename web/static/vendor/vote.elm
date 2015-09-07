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

type alias Model =
  { items : List Item
  , field : String
  , selectedItemType : ItemType
  , uid : Int
  }

type alias Item =
  { uid : Int
  , description : String
  , itemType : ItemType
  , points : Int
  }

type ItemType
  = Address
  | Feature

type alias Encoder a = a -> Encode.Value

decodeItemType : Json.Decoder ItemType
decodeItemType =
  Json.customDecoder Json.string strToItemType

encodeItem : Item -> Encode.Value
encodeItem item =
  Encode.object
    [ ("item", Encode.object
                [ ("description", Encode.string item.description)
                , ("item_type"  , Encode.string <| toString item.itemType)
                , ("points"     , Encode.int item.points)
                ])
    ]

postJson : Json.Decoder value -> String -> Http.Body -> Task Http.Error value
postJson decoder url body =
  Http.fromJson decoder
  <| Http.send Http.defaultSettings
      { verb = "POST"
      , headers = [("Content-Type", "application/json")]
      , url = url
      , body = body
      }

putJson : Json.Decoder value -> String -> Http.Body -> Task Http.Error value
putJson decoder url body =
  Http.fromJson decoder
  <| Http.send Http.defaultSettings
      { verb = "PUT"
      , headers = [("Content-Type", "application/json")]
      , url = url
      , body = body
      }

type Action
  = NoOp
  | NewItem String
  | CreateItem
  | Upvote Int
  | Downvote Int
  | ItemTypeSelected ItemType
  | UpdateItems (Maybe (List Item))

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
          }, Effects.batch [apiPostItem newItem, apiGetItems])

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
      let allItems = Maybe.withDefault [Item 0 "can't parse" Address 0] items
      in ({ model |
              uid   <- Maybe.withDefault model.uid <| List.maximum <| List.map (\i -> i.uid) allItems,
              items <- allItems }
         , Effects.none)


upvote : Int -> List Item -> (List Item, Effects Action)
upvote id items =
  let upd i = { i | points <- i.points + 1 }
  in ( updateItems id items upd
     , updateItem id items upd)

downvote : Int -> List Item -> (List Item, Effects Action)
downvote id items =
  let upd i = { i | points <- i.points - 1 }
  in ( updateItems id items upd
     , updateItem id items upd)

updateItem : Int -> List Item -> (Item -> Item) -> Effects Action
updateItem id items f =
  case find (\i -> i.uid == id) items of
    Nothing -> Effects.none
    Just i  -> apiPutItem <| f i

updateItems : Int -> List Item -> (Item -> Item) -> List Item
updateItems id items f =
  let upd i = if i.uid == id then f i else i
  in List.map upd items

find : (a -> Bool) -> List a -> Maybe a
find f xs =
  let first = List.take 1 <| List.filter f xs
  in case first of
      []  -> Nothing
      [x] -> Just x

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "vote-wrapper" ]
    [ header
      [ id "entry-header" ]
      [ h1 [] [ text "Address & Feature voting!" ]]
      , select
          [ id "item-type"
          , on "input" targetValue (Signal.message address << selectedItemType)
          ]
          [ option [] [ text <| toString Address ]
          , option [] [ text <| toString Feature ]
          ]
      , input
          [ id "new-vote"
          , placeholder "What's needed?"
          , autofocus True
          , value model.field
          , name "newVote"
          , on "input" targetValue (Signal.message address << NewItem)
          , onEnter address CreateItem
          ]
          []
      , section
        [ id "vote-list" ]
        [ ul
          []
          (List.map
            (renderItem address)
            (List.sortBy
              (\i -> -i.points)
              (List.filter
                (\i -> i.itemType == model.selectedItemType)
                model.items)))
        ]
    ]

strToItemType : String -> Result String ItemType
strToItemType str =
  case str of
    "Feature" -> Ok Feature
    "Address" -> Ok Address
    x         -> Err ("Unknown item type: " ++ x)

selectedItemType : String -> Action
selectedItemType str =
  case strToItemType str of
    Ok itemType -> ItemTypeSelected itemType
    Err _       -> ItemTypeSelected Address

renderItem : Signal.Address Action -> Item -> Html
renderItem address item =
  div
    [ class "item" ]
    [ div [ class "left" ] [ text (toString item.points) ]
    , div
        [ class "left" ]
        [ div [ class "arrow-up", onClick address (Upvote item.uid) ] []
        , div [ class "arrow-down", onClick address (Downvote item.uid) ] []
        ]
    , div [ class "left" ] [ text <| item.description ++ "-" ++ (toString item.itemType)]
    ]
--li [] [ text item.description ]

onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"

initModel : (Model, Effects Action)
initModel =
  ( { items = []
    , field = ""
    , selectedItemType = Address
    , uid   = 0
    }
  , apiGetItems
  )

baseURI : String
baseURI = "http://localhost:4000"

apiGetItems : Effects Action
apiGetItems =
  let url = baseURI ++ "/api/items"
  in Http.get itemList url
       |> Task.toMaybe
       |> Task.map UpdateItems
       |> Effects.task

apiPostItem : Item -> Effects Action
apiPostItem item =
  let url = baseURI ++ "/api/items"
  in postJson decodeItem url (toJsonBody encodeItem item)
       |> Task.toMaybe
       |> Task.map (\_ -> NoOp)
       |> Effects.task

apiPutItem : Item -> Effects Action
apiPutItem item =
  let url = baseURI ++ "/api/items/" ++ toString item.uid
  in putJson decodeItem url (toJsonBody encodeItem item)
       |> Task.toMaybe
       |> Task.map (\_ -> NoOp)
       |> Effects.task


toJsonBody : Encoder a -> a -> Http.Body
toJsonBody encoder x = Http.string <| Encode.encode 0 (encoder x)

decodeItem : Json.Decoder Item
decodeItem =
  Json.object4 Item
    ("id"          := Json.int)
    ("description" := Json.string)
    ("item_type"   := decodeItemType)
    ("points"      := Json.int)

itemList : Json.Decoder (List Item)
itemList = Json.at ["data"]
            (Json.list decodeItem)

app : StartApp.App Model
app =
  StartApp.start
    { init = initModel
    , update = update
    , view = view
    , inputs = []
    }

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

main : Signal Html
main = app.html
