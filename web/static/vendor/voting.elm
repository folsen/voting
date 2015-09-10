module Main where

import Html exposing (Html)
import Effects exposing (Effects, Never)
import StartApp
import Task

import Voting.Action exposing (update)
import Voting.Api exposing (apiGetItems)
import Voting.Types exposing (emptyModel, Model, Action)
import Voting.View exposing (view)

--------------
-- PLUMBING --
--------------

-- | Create a default model, used as the world state when the application
--   starts, also declare which effects to execute on startup
initModel : (Model, Effects Action)
initModel =
  ( emptyModel
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
