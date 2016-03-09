module DoubleCounter (Model, init, Action, update, view, viewWithRemoveButton, Context) where

-- Imports
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

-- Model

type alias Model = Int

init : Int -> Model
init count = count


-- Update

type Action = Increment | Decrement | DoubleIncrement | DoubleDecrement

update : Action -> Model -> Model
update action model =
  case action of
    Increment -> 
      model + 1
    Decrement -> 
      model - 1
    DoubleIncrement -> 
      model + 2
    DoubleDecrement -> 
      model - 2


-- View

view : Signal.Address Action -> Model -> Html
view address model =
 div [ mainStyle]
 [ button [ onClick address DoubleIncrement ] [ text "++"]
 , p [] [ text ""]
 , button [ onClick address Increment ] [ text "+" ]
 , div [ ] [ text (toString model) ]
 , button [ onClick address Decrement ] [ text "-" ]
 , p [] [ text ""]
 , button [ onClick address DoubleDecrement ] [ text "- -"]
 ]


type alias Context =
    { actions : Signal.Address Action
    , remove : Signal.Address ()
    }

viewWithRemoveButton : Context -> Model -> Html
viewWithRemoveButton context model =
 div [ mainStyle]
 [ button [ onClick context.actions DoubleIncrement ] [ text "++"]
 , p [] [ text ""]
 , button [ onClick context.actions Increment ] [ text "+" ]
 , div [ ] [ text (toString model) ]
 , button [ onClick context.actions Decrement ] [ text "-" ]
 , p [] [ text ""]
 , button [ onClick context.actions DoubleDecrement ] [ text "- -"]
 , div [] []
 , button [ onClick context.remove() ] [ text "X" ]
 ]
 
mainStyle : Attribute
mainStyle =
  style
  [ ("font-size", "20px")
  , ("font-family", "calibri")
  , ("display", "block")
  , ("width", "200px")
  , ("text-align", "center")
  , ("border", "1px solid black")
  ]

{--
countStyle : Attribute
countStyle =
  style
  [ ("font-size", "20px")
  , ("font-family", "calibri")
  , ("display", "block")
  , ("width", "200px")
  , ("text-align", "center")
  , ("border", "1px solid black")
  ]
--}