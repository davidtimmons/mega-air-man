module MegaAirMan (main) where

-- <MegaAirMan> Modules
import AirMan
import Arena

-- Elm Modules
import Effects
import Html exposing (Html)
import StartApp
import Task exposing (Task)

{-|
-}


{-| This function uses a Config record to return an App record composed of
three different signals. <main> uses the <html> signal. <model> is not often
needed according to the official <StartApp#2.0.2> documentation. The <tasks>
port uses the <tasks> value.

    -- <Task Never ()> is a task that never fails and returns nothing when
    -- it succeeds (<()> is an empty I/O action result in Haskell.)
    type alias App model =
      { html : Signal Html
      , model : Signal model
      , tasks : Signal (Task Never ())
      }
-}
app : StartApp.App Arena.Model
app =
  StartApp.start
    { init = Arena.init
    , update = Arena.update
    , view = Arena.view
    , inputs = []
    }


{-|
-}
main : Signal Html
main =
  app.html


{-| Tasks run in response to events. They are similar to signals but work with
code and programs external to Elm by sending and receiving data through a port.
-}
port tasks : Signal (Task Effects.Never ())
port tasks =
  app.tasks

{--
main : Signal Html
main =
  start
    { model = AirMan.init 0 0 0 0 AirMan.Right
    , update = AirMan.update
    , view = AirMan.view
    }
--}
