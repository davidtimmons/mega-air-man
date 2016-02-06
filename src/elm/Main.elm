module MegaAirMan (main) where

import AirMan
import Arena
import Html exposing (Html)
import StartApp.Simple exposing (start)

{-|
-}

{-|
-}
main : Signal Html
main =
  start
    { model = Arena.init
    , update = Arena.update
    , view = Arena.view
    }

{--
main : Signal Html
main =
  start
    { model = AirMan.init 0 0 0 0 AirMan.Right
    , update = AirMan.update
    , view = AirMan.view
    }
--}
