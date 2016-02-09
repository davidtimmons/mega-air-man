module Arena
  ( Model
  , init
  , update
  , view
  , playNextFrame
  ) where

-- <MegaAirMan> Modules
import Utilities

-- Elm Modules
import Effects exposing (Effects)
import Html exposing (div, Html)
import Html.Attributes exposing (classList, title)
import Signal exposing (Signal)
import Time

{-| This module displays and animates the Air Man arena background.

# Model
@docs Model, Frame, init

# Update
@docs Action, update

# View
@docs view

# Signals
@docs playNextFrame
-}


-----------
-- MODEL --
-----------

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


{-| This data type captures the CSS class associated with the
current animation frame.

    Frame "icon-mm2-airman-arena1"
-}
type alias Frame =
  String


{-| Set model defaults to include all animation frames in play order.
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

{-| Trigger the next animation frame in the cycle with this Action.
-}
type Action =
  NextFrame


{-| Plays the background animation by changing the CSS classes that correspond
to animation frames in the sprite sheet.
-}
update : Action -> Model -> (Model, Effects a)
update action model =
  Utilities.noFx <|
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

{-| Create the background as a div image.
Note: <Signal.Address> input comes from the <StartApp Config.view> data type.
-}
view : Signal.Address Action -> Model -> Html
view address model =
  div -- Parent container.
    [ classList
        [ ("icon", True)
        , (model.currentFrame, True)
        , ("rel", True)
        ]
    , title "Mega Air Man Arena"
    ]
    [ div -- Active area container.
      [ classList
        [ ("abs", True)
        , ("active-area", True)
        , ("active-pos", True)
        ]
      ]
      []
    ]


-------------
-- SIGNALS --
-------------

{-| Trigger the animation cycle. Prefer <Signal> to <Effects.tick> for this
animation because the program simulates choppy NES animation rather than
the duration-based approached possible with <Effects.tick>.
-}
playNextFrame : Signal Action
playNextFrame =
  Signal.map (\_ -> NextFrame) (Time.fps 3.5)
