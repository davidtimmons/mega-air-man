module Shared where

import Char
import Effects exposing (Effects)
import Keyboard
import Set exposing (Set)
import Signal exposing (Signal)
import Time exposing (Time)

{-| This module collects shared functions and data used in other modules.

# Types
@docs ArrowKeys, Buttons, DTime, Frame

# Effects
@docs noFx, maybeTick

# Signals
@docs sampleKeyboardInput
-}


-----------
-- TYPES --
-----------

{-| Represents Signal output from the Keyboard module. Positive values indicate
up and right. Negative values indicate bottom and left. Zero is no input. See
<Keyboard> for further explanation.
-}
type alias ArrowKeys =
  { x : Int
  , y : Int
  }


{-| Contains the set of keys that have been pressed.
-}
type alias Buttons
  = Set Char.KeyCode


{-| Represents a time measurement delta generated by the Signal section.
-}
type alias DTime
  = Float


{-| This data type captures the CSS class associated with the
current animation frame.

    Frame "icon-mm2-airman-arena1"
-}
type alias Frame
  = String


-------------
-- EFFECTS --
-------------

{-| A convenience function that indicates an update has no associated Effects.
Source: Elm Effects documentation.

    noFx <| ...
-}
noFx : a -> (a, Effects b)
noFx model =
  (model, Effects.none)


{-| Begin an animation sequence.
-}
maybeTick : Bool -> (Time -> a) -> b -> (b, Effects a)
maybeTick shouldPlay action model =
  if shouldPlay == True
  then ( model, Effects.tick action )
  else noFx model


-------------
-- SIGNALS --
-------------

{-| Samples keyboard input to determine elapsed time and key presses.
-}
sampleKeyboardInput : ((DTime, ArrowKeys, Buttons) -> a) -> Signal a
sampleKeyboardInput action =
  let
    delta = Signal.map (\t -> t/20) (Time.fps 30)
    arrows = Keyboard.arrows
    keys = Signal.dropRepeats Keyboard.keysDown
  in
    Signal.sampleOn delta (Signal.map3 (,,) delta arrows keys)
    |> Signal.map (\(dt, arr, btn) -> action (dt, arr, btn))
