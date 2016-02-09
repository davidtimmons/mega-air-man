module MegaAirMan (main) where

-- <MegaAirMan> Modules
import AirMan
import Arena

-- Elm Modules
import Effects
import Html exposing (Html)
import StartApp
import Task exposing (Task)

-- TEMP!!
import Time

{-| This module orchestrates the application. It initializes state, defines
input signals to feed into the app, and opens ports that work with Tasks.

# Elm Architecture
@docs app, main, tasks
-}


{-| <app> uses a Config record to return an App record composed of
three different signals: <main>, <model>, <tasks>. <model> is not often
needed according to the official <StartApp#2.0.2> documentation.

    -- <Task Never ()> is a task that never fails and returns nothing when
    -- it succeeds (<()> is an empty I/O action result in Haskell.)
    type alias App model =
      { html : Signal Html -- Used in <main>.
      , model : Signal model -- Not used.
      , tasks : Signal (Task Never ()) -- Used in <tasks>.
      }
-}
app : StartApp.App Arena.Model
app =
  StartApp.start
    { init = Arena.init
    , update = Arena.update
    , view = Arena.view
    -- <inputs> captures both vanilla Signals and external port data.
    , inputs =
        [ Arena.playNextFrame
        ]
    }


{-| <main> is the application entry point. It captures the current visual
representation of the app.
-}
main : Signal Html
main =
  app.html


{-| <tasks> opens a port that runs Task(s) in response to events. They are
similar to signals but work with code external to Elm by sending and receiving
data through a port (similar to FFI in Haskell).
-}
port tasks : Signal (Task Effects.Never ())
port tasks =
  app.tasks
