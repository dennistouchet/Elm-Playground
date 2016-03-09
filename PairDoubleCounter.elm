module PairDoubleCounter where

-- Imports
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import DoubleCounter

-- Model

type alias Model = { leftCounter : DoubleCounter.Model, rightCounter : DoubleCounter.Model}


init : Int -> Int -> Model
init left right = { leftCounter = DoubleCounter.init left, rightCounter = DoubleCounter.init right}


-- Update

type Action 
 = Clear | Left DoubleCounter.Action | Right DoubleCounter.Action

update : Action -> Model -> Model
update action model = case action of
 Clear -> init 0 0

 Left action ->
 { model |
 leftCounter = DoubleCounter.update action model.leftCounter
 }

 Right action ->
 { model |
 rightCounter = DoubleCounter.update action model.rightCounter
 }


-- View

view : Signal.Address Action -> Model -> Html
view address model =
 div [ mainStyle]
 [ DoubleCounter.view (Signal.forwardTo address Left) model.leftCounter
 , DoubleCounter.view (Signal.forwardTo address Right) model.rightCounter
 , button [ onClick address Clear ] [ text "Clear"]
 ]


mainStyle : Attribute
mainStyle =
  style
  [ ("font-size", "20px")
  , ("font-family", "helvetica")
  , ("display", "inline-block")
  , ("width", "200px")
  , ("text-align", "center")
  , ("border", "1px solid blue")
  , ("margin", "50px 50px")
  , ("padding", "50px 50px")
  ]