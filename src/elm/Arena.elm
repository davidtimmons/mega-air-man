module Arena
  ( Model
  , Action(..)
  , init
  , update
  , view
  , playNextFrame
  ) where

import Effects exposing (Effects)
import Html exposing (div, Html)
import Html.Attributes exposing (classList, title)
import Signal exposing (Signal)
import Time

{-|
-}


-----------
-- MODEL --
-----------

{-| This data type captures the CSS class associated with the
current animation frame.

    Frame "icon-mm2-airman-arena1"
-}
type alias Frame =
  String


{-| The model holds frame information associated with the background animation.
-}
type alias Model =
  { currentFrameNumber : Int
  , currentFrame : Frame
  , f1 : Frame
  , f2 : Frame
  , f3 : Frame
  , f4 : Frame
  }


{-|
-}
init : (Model, Effects a)
init =
  (
    { currentFrameNumber = 1
    , currentFrame = "icon-mm2-airman-arena1"
    , f1 = "icon-mm2-airman-arena1"
    , f2 = "icon-mm2-airman-arena2"
    , f3 = "icon-mm2-airman-arena3"
    , f4 = "icon-mm2-airman-arena2"
    }
    , Effects.none
  )


------------
-- UPDATE --
------------

{-|
-}
type Action
  = NextFrame


{-| Source: Elm Effects documentation.
-}
noFx : a -> (a, Effects b)
noFx model =
  (model, Effects.none)


{-|
-}
update : Action -> Model -> (Model, Effects a)
update action model =
  noFx <|
  case action of
    NextFrame ->
      let
        nextNumber = 1 + (model.currentFrameNumber % 4)
        nextFrame =
          case nextNumber of
            2 ->
              model.f2

            3 ->
              model.f3

            4 ->
              model.f4

            _ ->
              model.f1

      in
        { model | currentFrameNumber = nextNumber
                , currentFrame = nextFrame
        }


----------
-- VIEW --
----------

-- signal.address comes from the startapp config.view data type
{-|
-}
view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ classList
        [ ("icon", True)
        , (model.currentFrame, True)
        ]
    , title "Air Man Arena"
    ]
    []


-------------
-- SIGNALS --
-------------

{-|
-}
playNextFrame : Signal Action
playNextFrame =
  Signal.map (\_ -> NextFrame) (Time.fps 3.5)
