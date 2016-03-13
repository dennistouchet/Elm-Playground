module GiphyApiRandomPair where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)

import GiphyApiRandom

-- MODEL

type alias Model =
    { left : GiphyApiRandom.Model
    , right : GiphyApiRandom.Model
    }


init : String -> String -> (Model, Effects Action)
init leftTopic rightTopic =
    let 
        (left, leftFx) = GiphyApiRandom.init leftTopic
        (right, rightFx) = GiphyApiRandom.init rightTopic
    in
        ( Model left right
        , Effects.batch
            [ Effects.map Left leftFx
            , Effects.map Right rightFx
            ]
        )


-- UPDATE

type Action
    = Left GiphyApiRandom.Action
    | Right GiphyApiRandom.Action


update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Left act ->
            let 
                (left, fx) = GiphyApiRandom.update act model.left
            in
                ( Model left model.right
                , Effects.map Left fx
                )

        Right act ->
            let
                (right, fx) = GiphyApiRandom.update act model.right
            in
                ( Model model.left right
                , Effects.map Right fx
                )


-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
    div [ style [ ("", "") ] ]
        [ GiphyApiRandom.view (Signal.forwardTo address Left) model.left
        , GiphyApiRandom.view (Signal.forwardTo address Right) model.right
        ]