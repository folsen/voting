module Voting.Api where

import Http
import Task exposing (toMaybe, map, Task)
import Json.Decode as Json
import Effects exposing (Effects, Never)

import Voting.Types exposing (..)
import Voting.Json exposing (..)

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
  in postJson singleItem url (toJsonBody encodeItem item)
       |> Task.toMaybe
       |> Task.map InsertItem
       |> Effects.task

-- | Put an item (updating it)
apiPutItem : Item -> Effects Action
apiPutItem item =
  let url = "/api/items/" ++ toString item.uid
  in putJson singleItem url (toJsonBody encodeItem item)
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
