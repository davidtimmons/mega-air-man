module AirMan
  ( Model
  , Action(HandleInput, NextFrame)
  , init
  , update
  , view
  ) where

-- <MegaAirMan> Modules
import Shared exposing (Frame)

-- Elm Modules
import Effects exposing (Effects)
import Html exposing (Html, div)
import Html.Attributes exposing (classList, style)
import Keyboard
import Signal exposing (Signal)
import Time exposing (Time)

{-| This module controls all actions and all state associated with the Air Man
sprite. <Arena> initializes <AirMan>.

# Model
@docs ...

# Update
@docs ...

# view
@docs ...
-}


-----------
-- MODEL --
-----------

{-| Represents the 2D position of a sprite. <x> and <y> are the coordinate
position. <vx> and <vy> are the velocity along the axes. <dir> is the direction
this sprite faces.

    -- At any given moment in time, a jumping sprite may look like this.
    AirMan : Model
    AirMan =
      { ani = {...}
      , x = 0.0
      , y = 10.7
      , vx = 0.5
      , vy = 6.0
      , dir = Right
      }
-}
type alias Model =
  { ani : AnimationState
  , x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Direction
  }


{-| This model holds all animation state associated with the Air Man sprite.
-}
type alias AnimationState =
  { previousTime : Time
  , elapsedTime : Time
  , currentFrame : Frame
  , stand : Frame
  , jump : Frame
  , shootTotalFrames : Int
  , shootF1 : Frame
  , shootF2 : Frame
  , shootF3 : Frame
  }


{-| Represents the direction this sprite faces within a 2D coordinate system.

    leftDirection : Direction
    leftDirection = Left
-}
type Direction
  = Left
  | Right


{-| Create the initial sprite model.
-}
init : (Model, Effects a)
init =
  Shared.noFx <|
  { ani =
      { previousTime = 0.0
      , elapsedTime = 0.0
      , currentFrame = "icon-mm2-airman-stand"
      , stand = "icon-mm2-airman-stand"
      , jump = "icon-mm2-airman-jump"
      , shootTotalFrames = 30 -- Cycle (F1 and F2) then play (F3).
      , shootF1 = "icon-mm2-airman-shoot1"
      , shootF2 = "icon-mm2-airman-shoot2"
      , shootF3 = "icon-mm2-airman-shoot3"
      }
  , x = 0.0
  , y = 0.0
  , vx = 0.0
  , vy = 0.0
  , dir = Right
  }


------------
-- UPDATE --
------------

{-| Trigger the next animation frame in the desired cycle with this Action type.
-}
type Action
  = NextFrame Time
  | HandleInput (Shared.DTime, Shared.ArrowKeys)
  | Stand
  | Jump
  | Shoot


{-| The cloud background animation plays at about 3.5 FPS.
-}
playRate : Time
playRate =
  100 * Time.millisecond


{-| Updates state to cycle to the next animation frame.
-}
updateAnimationState : AnimationState -> Action -> Time -> AnimationState
updateAnimationState ani action clockTime =
  case action of
    Jump ->
      { ani | currentFrame = ani.jump }

    Shoot ->
      let
        -- Capture the total time this animation cycle has been playing.
        duration = ani.elapsedTime + (clockTime - ani.previousTime)

        -- Test whether the animation cycle has finished.
        newElapsedTime =
          if duration > (toFloat ani.shootTotalFrames) * playRate
          then 0.0
          else duration

        -- Get the next frame in the cycle.
        spin = toFloat ani.shootTotalFrames / 2

        nextFrame =
          if
            duration < spin * playRate && not (ani.currentFrame == ani.shootF1)
          then
            ani.shootF1
          else if
            duration < spin * playRate && not (ani.currentFrame == ani.shootF2)
          then
            ani.shootF2
          else
            ani.shootF3

      in
        { ani | previousTime = clockTime
              , elapsedTime = newElapsedTime
              , currentFrame = nextFrame
        }

    _ -> -- Default to the stand image.
      { ani | currentFrame = ani.stand }


{-| Update the Air Man sprite model in response to Signals and Effects.
The <NextFrame> action is initiated by <Arena>.
-- TODO Incorporate direction, translate movements, shoot timing
-}
update : Action -> Model -> (Model, Effects a)
update action model =
  Shared.noFx <|
  -- TODO Write cases
  case action of
    NextFrame clockTime ->
      -- TODO Testing
      --{ model | ani = updateAnimationState model.ani Shoot clockTime }
      model

    HandleInput (dTime, arrowKeys) ->
      model
        |> applyGravity dTime
        |> setJump arrowKeys
        |> setDirection arrowKeys
        |> applyPhysics dTime

    Jump ->
      { model | ani = updateAnimationState model.ani action 0.0 }

    Shoot ->
      { model | ani = updateAnimationState model.ani action 0.0 }

    _ ->
      { model | ani = updateAnimationState model.ani action 0.0 }


{-| Apply gravity if jumping.
-}
applyGravity : Shared.DTime -> Model -> Model
applyGravity dTime model =
  let
    newVy =
      if
        model.y > 0.0
      then
        model.vy - dTime/4.0
      else
        0.0

  in
    { model | vy = newVy }


{-| Respond to a jump command riding on the input Signal. <vy> controls the
vertical height Air Man will jump.
-}
setJump : Shared.ArrowKeys -> Model -> Model
setJump arrowKeys model =
  if
    (arrowKeys.y > 0 && model.vy == 0.0)
  then
    { model | vy = 7.4 }
  else
    model


{-| Respond to a movement command riding on the input Signal. <vx> controls
how far Air Man moves or jumps along the x-axis.
-}
setDirection : Shared.ArrowKeys -> Model -> Model
setDirection arrowKeys model =
  let
    newVx = 2.8 * toFloat arrowKeys.x

    newDir =
      if
        arrowKeys.x < 0
      then
        Left
      else if
        arrowKeys.x > 0
      then
        Right
      else
        model.dir

  in
    { model | vx = newVx, dir = newDir }


{-| Apply momentum to the moving sprite model. Air Man can only jump!
-}
applyPhysics : Shared.DTime -> Model -> Model
applyPhysics dTime model =
  let
    newY = max 0 (model.y + dTime * model.vy)

    newX =
      if
        newY > 0
      then
        -- Bound him to the right edge of the active area.
        min 481 (model.x + dTime * model.vx)
      else
        -- Bound him to the left edge of the active area.
        max 0 model.x

  in
    { model | x = newX, y = newY }


----------
-- VIEW --
----------

{-| Move the sprite according to the model state values.
-}
transformStyles : Model -> List (String, String)
transformStyles model =
  let
    translateX =
      if
        model.x <= 0 && model.dir == Left
      then
        ""
      else if
        model.x <= 0 && model.dir == Right
      then
        "translateX(0px) "
      else
        "translateX(" ++ toString model.x ++ "px) "

    translateY =
      if
        model.y <= 0
      then
        ""
      else
        "translateY(-" ++ toString model.y ++ "px) "

    scaleX =
      if
        (model.ani.currentFrame /= model.ani.shootF3) && model.dir == Right
      then
        "scaleX(-1) "
      else
        ""

  in
    [ ("transform", translateX ++ translateY ++ scaleX) ]


{-| Display the Air Man sprite.
-}
view : Signal.Address Action -> Model -> Html
view address model =
  div -- Air Man sprite.
    [ classList
        [ ("abs", True)
        , ("btm", True)
        , ("icon", True)
        , (model.ani.currentFrame, True)
        , ("airman-shoot3-left"
          , model.ani.currentFrame == model.ani.shootF3 &&
            model.dir == Left
          )
        , ("airman-shoot3-right"
          , model.ani.currentFrame == model.ani.shootF3 &&
            model.dir == Right
          )
        ]
    , style (transformStyles model)
    ]
    [ -- Tornado sprites.
    ]
