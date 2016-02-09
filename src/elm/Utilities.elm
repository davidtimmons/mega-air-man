module Utilities where

import Effects exposing (Effects)

{-| This module collects generic utility functions shared by other modules.

# Signals & Effects
@docs noFx
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
