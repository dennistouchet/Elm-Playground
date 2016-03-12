-- Imports
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp

-- Main
main = 
   StartApp.start { model = 0 , view = view, update = update }


-- Model

type alias Model = Int


-- View

view : Signal.Address Action -> Model -> Html
view address model =
 div [ ]
 [ button [ onClick address DoubleDecrement ] [ text "- -" ]
 , p [] [ text ""]
 , button [ onClick address Decrement ] [ text "-" ]
 , div [ ] [ text (toString model) ]
 , button [ onClick address Increment ] [ text "+" ]
 , p [] [ text ""]
 , button [ onClick address DoubleIncrement ] [ text "++"]
 ]


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