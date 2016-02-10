module Shared where

import Effects exposing (Effects)
import Keyboard
import Signal exposing (Signal)
import Time exposing (Time)

{-| This module collects shared functions and data used in other modules.

# Types
@docs Frame, ArrowKeys, DTime

# Effects
@docs noFx

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


-------------
-- SIGNALS --
-------------

{-| Samples keyboard input to determine elapsed time and key presses.
-}
sampleKeyboardInput : ((DTime, ArrowKeys) -> a) -> Signal a
sampleKeyboardInput action =
  let
    delta = Signal.map (\t -> t/20) (Time.fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
    |> Signal.map (\(dt, kb) -> action (dt, kb))
