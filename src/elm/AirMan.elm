module AirMan
  ( Model
  , Action(HandleInput)
  , init
  , update
  , view
  ) where

-- <MegaAirMan> Modules
import Shared exposing (Frame)

-- Elm Modules
import Char
import Effects exposing (Effects)
import Html exposing (Html, div)
import Html.Attributes exposing (classList, style)
import Keyboard
import Set exposing (Set)
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
  , shootIsPlaying : Bool
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
      , shootIsPlaying = False
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
  | HandleInput (Shared.DTime, Shared.ArrowKeys, Shared.Buttons)
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
        duration = max 0.0 (ani.elapsedTime + (clockTime - ani.previousTime))

        -- Test whether the animation cycle has finished.
        newElapsedTime =
          if duration > (toFloat ani.shootTotalFrames) * playRate
          then 0.0
          else duration

        -- Set proportion of animation sequence needed for the spin sequence.
        spin = toFloat ani.shootTotalFrames / 2

        -- Determine the next frame in the shooting sequence.
        nextShootFrame =
          if duration < spin * playRate && ani.currentFrame /= ani.shootF1
          then ani.shootF1
          else if duration < spin * playRate && ani.currentFrame /= ani.shootF2
          then ani.shootF2
          else ani.shootF3

        -- End shooting sequence after it has finished playing.
        isShooting =
          if newElapsedTime == 0.0 && nextShootFrame == ani.shootF3
          then False
          else True

        -- Determine the next sprite image.
        nextFrame =
          if isShooting
          then nextShootFrame
          else ani.stand

      in
        { ani | previousTime = clockTime
              , elapsedTime = newElapsedTime
              , currentFrame = nextFrame
              , shootIsPlaying = isShooting
        }

    _ -> -- Default to the stand image.
      { ani | currentFrame = ani.stand }


{-| Update the Air Man sprite model in response to Signals and Effects.
-}
update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NextFrame clockTime ->
      if -- Plays the shoot animation sequence.
        model.ani.shootIsPlaying
      then
        ( { model | ani = updateAnimationState model.ani Shoot clockTime }
        , Effects.tick NextFrame
        )
      else
        Shared.noFx model

    HandleInput (dTime, arrowKeys, buttons) ->
      let -- React to user keyboard input if the shoot animation is not playing.
        (didShoot, didJump) =
          Set.foldl (\key (isBtn1, isBtn2) -> areButtons key (isBtn1, isBtn2))
            (False, False) buttons

      in
        if
          model.ani.shootIsPlaying
        then
          Shared.noFx model
        else
          model
          |> applyGravity dTime
          |> setShoot didShoot
          |> setJump didJump
          |> setDirection arrowKeys
          |> applyPhysics dTime
          |> Shared.startAnimation didShoot NextFrame

    _ -> -- Make no changes if the <Action> is unrecognized.
      Shared.noFx model


{-| Determines if a keypress matches the desired button.
-}
isKeyButton : Char.KeyCode -> Char -> Bool
isKeyButton key button =
  Char.toLower (Char.fromCode key) == button


{-| Determine if a keypress matches any desired button.
-}
areButtons : Char.KeyCode -> (Bool, Bool) -> (Bool, Bool)
areButtons key (isBtn1, isBtn2) =
  (isBtn1 || isKeyButton key 'a', isBtn2 || isKeyButton key 's')


{-| Apply gravity if jumping. Revert to the standing animation upon landing.
-}
applyGravity : Shared.DTime -> Model -> Model
applyGravity dTime model =
  if
    model.y > 0.0
  then
    { model | vy = model.vy - dTime/4.0 }
  else
    { model | ani = updateAnimationState model.ani Stand 0.0
            , vy = 0.0
    }


{-| Play the shooting animation in response to the correct button press.
-}
setShoot : Bool -> Model -> Model
setShoot didShoot model =
  if
    didShoot == True && model.ani.shootIsPlaying == False && model.vy == 0.0
  then
    let
      ani = model.ani
      newAni = { ani | shootIsPlaying = True }
    in
      { model | ani = newAni }
  else
    model


{-| Respond to a jump command riding on the input Signal. <vy> controls the
vertical height Air Man will jump.
-}
setJump : Bool -> Model -> Model
setJump didJump model =
  if
    didJump == True && model.vy == 0.0 && model.ani.shootIsPlaying == False
  then
    { model | ani = updateAnimationState model.ani Jump 0.0
            , vy = 7.4
    }
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
      if arrowKeys.x < 0
      then Left
      else if arrowKeys.x > 0
      then Right
      else model.dir

  in
    { model | vx = newVx, dir = newDir }


{-| Apply momentum to the moving sprite model. Air Man can only jump!
-}
applyPhysics : Shared.DTime -> Model -> Model
applyPhysics dTime model =
  let
    newY = max 0 (model.y + dTime * model.vy)

    newX =
      if newY > 0
      -- Bound him to the right edge of the active area.
      then min 481 (model.x + dTime * model.vx)
      -- Bound him to the left edge of the active area.
      else max 0 model.x

  in
    { model | x = newX, y = newY }


----------
-- VIEW --
----------

{-| Move the sprite according to the model state values. Adjust positions where
needed to correctly line up the sprite animation.
-}
transformStyles : Model -> List (String, String)
transformStyles model =
  let
    -- Determine which way the sprite is facing.
    isRight = model.dir == Right

    -- Offset the jumping animation frame.
    jump = model.ani.jump
    jumpOffset =
      if isRight
      then model.x - 12.0
      else max 0.0 (model.x)

    -- Offset the shoot animation frames.
    currentFrame = model.ani.currentFrame
    shootF1 = model.ani.shootF1
    shootF2 = model.ani.shootF2
    shootF3 = model.ani.shootF3
    isShooting =
      currentFrame == shootF1 ||
      currentFrame == shootF2 ||
      currentFrame == shootF3
    shootOffset =
      if currentFrame == shootF1
        then if isRight
          then model.x - 10.0 -- Right offset.
          else model.x +  0.0 -- Left offset.
      else if currentFrame == shootF2
        then if isRight
          then model.x - 10.0 -- Right offset.
          else model.x +  0.0 -- Left offset.
      else
        if isRight            -- currentFrame == shootF3
          then model.x +  0.0 -- Right offset.
          else model.x - 10.0 -- Left offset.

    -- Move the sprite along the x-axis.
    translateX =
      -- Align jumps.
      if currentFrame == jump
      then "translateX(" ++ toString jumpOffset ++ "px) "
      -- Align shoot.
      else if isShooting
      then "translateX(" ++ toString shootOffset ++ "px) "
      -- Reduce wall clipping.
      else "translateX(" ++ toString (max 0 model.x) ++ "px) "

    -- Move the sprite along the y-axis.
    translateY = "translateY(-" ++ toString model.y ++ "px) "

    -- Flip the sprite according to the direction it is facing.
    scaleX =
      if isRight
      then "scaleX(-1) "
      else ""

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
        ]
    , style (transformStyles model)
    ]
    [ -- Tornado sprites.
    ]
