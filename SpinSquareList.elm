module SpinSquareList where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import SpinSquare


-- MODEL

type alias Model =
    { angle : Float
    , animationState : AnimationState
    , uid : Int
    }


init : (Model, Effects Action)
init =
--TODO


-- UPDATE

type Action
    = Left SpinSquare.Action
    | Right SpinSquare.Action


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
-- TODO



-- VIEW

(=>) = (,)


view : Signal.Address Action -> Model -> Html
view address model =
-- TODO