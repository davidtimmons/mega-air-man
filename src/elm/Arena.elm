module Arena
  ( init
  , update
  , view
  ) where

import Html exposing (..)
import Html.Attributes exposing (classList, title)
import Signal


-----------
-- MODEL --
-----------

-- animation frame css class
type alias Frame =
  String


-- animation frame css class
type alias Model =
  { current : Frame
  , f1 : Frame
  , f2 : Frame
  , f3 : Frame
  }


init : Model
init =
  { current = "icon-mm2-airman-arena1"
  , f1 = "icon-mm2-airman-arena1"
  , f2 = "icon-mm2-airman-arena2"
  , f3 = "icon-mm2-airman-arena3"
  }


------------
-- UPDATE --
------------

type Action =
  CycleFrame Int


update : Action -> Model -> Model
update action model =
  case action of
    CycleFrame 1 ->
      { model | current = model.f2 }

    CycleFrame 2 ->
      { model | current = model.f3 }

    CycleFrame _ ->
      { model | current = model.f1 }


----------
-- VIEW --
----------

-- signal.address comes from the startapp config.view data type
view : Signal.Address Action -> Model -> Html
view address model =
  div
    [
      classList [
        ("icon", True),
        (model.current, True)
      ],
      title "Air Man Arena"
    ]
    []
