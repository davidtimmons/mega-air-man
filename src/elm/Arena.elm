module Arena
  ( Model
  , init
  , update
  , view
  , sampleKeyboardInput
  ) where

-- <MegaAirMan> Modules
import AirMan
import Shared exposing (Frame)

-- Elm Modules
import Effects exposing (Effects)
import Html exposing (Html, div)
import Html.Attributes exposing (classList, title)
import Signal exposing (Signal)
import Time exposing (Time)

{-| This module displays and animates the Air Man arena background.

# Model
@docs Model, AnimationState, init

# Update
@docs Action, playRate, updateAnimationState, update

# View
@docs view
-}


-----------
-- MODEL --
-----------

{-| This model holds all animation state associated with the background sprite.
-}
type alias Model =
  { ani : AnimationState
  , airman : AirMan.Model
  }


{-| This model holds all animation state associated with the background sprite.
-}
type alias AnimationState =
  { previousTime : Time
  , elapsedTime : Time
  , totalFrames : Int
  , currentFrame : Frame
  , f1 : Frame
  , f2 : Frame
  , f3 : Frame
  , f4 : Frame
  }


{-| Set model defaults to include all animation frames in play order, and
create an Effects queue for all the effects generated by all the sprites
this module will manage.
-}
init : (Model, Effects Action)
init =
  let
    (airmanModel, airmanFx) = AirMan.init

  in
    ( { ani =
          { previousTime = 0.0
          , elapsedTime = 0.0
          , totalFrames = 4
          , currentFrame = "icon-mm2-airman-arena1"
          , f1 = "icon-mm2-airman-arena1"
          , f2 = "icon-mm2-airman-arena2"
          , f3 = "icon-mm2-airman-arena3"
          , f4 = "icon-mm2-airman-arena2"
          }
      , airman = airmanModel
      }
    , Effects.batch
        [ Effects.map SpriteAirMan airmanFx
        , Effects.tick NextFrame -- This <Action> captures <Time>.
        ]
    )


------------
-- UPDATE --
------------

{-| Trigger the next animation frame in the cycle with this Action and
orchestrate the main sprites. <SpriteAirMan> receives actions of some kind,
but those actions are unimportant. They are used to update the <AirMan> model
and send the new model back to the <AirMan> module. In other words, this is
a way to wire together the modules.
-}
type Action
  = NextFrame Time
  | HandleInput (Shared.DTime, Shared.ArrowKeys)
  | SpriteAirMan AirMan.Action


{-| The cloud background animation plays at about 3.5 FPS.
-}
playRate : Time
playRate =
  285 * Time.millisecond


{-| Updates state to cycle to the next animation frame.
-}
updateAnimationState : AnimationState -> Time -> AnimationState
updateAnimationState ani clockTime =
  let
    -- Capture the total time this animation cycle has been playing.
    duration = ani.elapsedTime + (clockTime - ani.previousTime)

    -- Test whether the animation cycle has finished.
    newElapsedTime =
      if duration > (toFloat ani.totalFrames) * playRate
      then 0.0
      else duration

    -- Get the next frame in the cycle.
    nextFrame =
      if duration < playRate
      then ani.f1
      else if duration < 2 * playRate
      then ani.f2
      else if duration < 3 * playRate
      then ani.f3
      else if duration < 4 * playRate
      then ani.f4
      else ani.f1

  in
    { ani | previousTime = clockTime
          , elapsedTime = newElapsedTime
          , currentFrame = nextFrame
    }


{-| Plays the background animation by changing the CSS classes that correspond
to animation frames in the sprite sheet.
-}
update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NextFrame clockTime ->
      let
        -- Update the cloud background sprite.
        newAni = updateAnimationState model.ani clockTime

        -- Update the Air Man sprite.
        (airman, airmanFx)
          = AirMan.update (AirMan.NextFrame clockTime) model.airman

      in
        -- Update the <Arena> model and and all sprite models it controls.
        ( Model newAni airman
        , Effects.batch
            [ Effects.map SpriteAirMan airmanFx
            , Effects.tick NextFrame
            ]
        )

    HandleInput a ->
      -- Pass input actions to the <AirMan> module.
      let
        (airman, airmanFx) = AirMan.update (AirMan.HandleInput a) model.airman

      in
        ( Model model.ani airman
        , Effects.map SpriteAirMan airmanFx
        )

    SpriteAirMan act ->
      {- Wire together the <AirMan> and <Arena> modules. The specific update
         actions are unimportant to the <Arena> module since <AirMan> has the
         responsibility to update the data.
      -}
      let
        (airman, airmanFx) = AirMan.update act model.airman
      in
        ( Model model.ani airman
        , Effects.map SpriteAirMan airmanFx
        )


----------
-- VIEW --
----------

{-| Display the sprite background as a <div> background image.
Note: <Signal.Address> input comes from the <StartApp Config.view> data type.
-}
view : Signal.Address Action -> Model -> Html
view address model =
  div -- Parent container.
    [ classList
        [ ("rel", True)
        , ("icon", True)
        , (model.ani.currentFrame, True)
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
      -- Send updates to <AirMan> and receive HTML here.
      [ AirMan.view (Signal.forwardTo address SpriteAirMan) model.airman
      ]
    ]

-------------
-- SIGNALS --
-------------

{-| Samples keyboard input to determine elapsed time and key presses. Fed into
<AirMan> in order to control the sprite.
-}
sampleKeyboardInput : Signal Action
sampleKeyboardInput =
  Shared.sampleKeyboardInput HandleInput
