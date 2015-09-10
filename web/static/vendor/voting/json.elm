module Voting.Json where

import Http
import Json.Decode as Json exposing ((:=))
import Json.Encode as Encode

import Voting.Types exposing (..)

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

-- | Convert a single JSON object (under the key `data`) to an Items
singleItem : Json.Decoder Item
singleItem = Json.at ["data"] decodeItem

-- | Convert a string to an ItemType or return an error
strToItemType : String -> Result String ItemType
strToItemType str =
  case str of
    "Feature" -> Ok Feature
    "Address" -> Ok Address
    x         -> Err ("Unknown item type: " ++ x)

