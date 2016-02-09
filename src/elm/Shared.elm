module Shared where

import Effects exposing (Effects)

{-| This module collects shared functions and data used in other modules.

# Signals & Effects
@docs noFx

# Types
@docs Frame
-}


-----------------------
-- SIGNALS & EFFECTS --
-----------------------

{-| A convenience function that indicates an update has no associated Effects.
Source: Elm Effects documentation.

    noFx <| ...
-}
noFx : a -> (a, Effects b)
noFx model =
  (model, Effects.none)


-----------
-- TYPES --
-----------

{-| This data type captures the CSS class associated with the
current animation frame.

    Frame "icon-mm2-airman-arena1"
-}
type alias Frame =
  String
