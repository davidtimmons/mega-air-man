import AirMan
import StartApp.Simple exposing (start)

{-|
-}

{-|
-}
main : Signal Html
main =
  start
    { model = AirMan.init 0 0 0 0 AirMan.Right
    , update = AirMan.update
    , view = AirMan.view
    }
