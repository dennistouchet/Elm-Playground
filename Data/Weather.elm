import Char
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (..)


-- VIEW

view : String -> Result String (List String) -> Html
view string result =
  let field =
        input
          [ placeholder "Enter a city"
          , value string
          , on "input" targetValue (Signal.message query.address)
          , myStyle
          ]
          []

      messages =
        case result of
          Err msg ->
              [ div  [ myStyle ] [ text msg ] ]

          Ok coords ->
              List.map (\coord -> div [ myStyle ] [ text coord ] ) coords
  in
      div [] (field :: messages)
     

myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]


-- WIRING
main =
   Signal.map2 view query.signal results.signal


query : Signal.Mailbox String
query =
  Signal.mailbox ""


results : Signal.Mailbox (Result String (List String))
results =
  Signal.mailbox (Err "A valid US City contain only letters.")


port requests : Signal (Task x ())
port requests =
  Signal.map lookupCity query.signal
    |> Signal.map (\task -> Task.toResult task `andThen` Signal.send results.address)


lookupCity : String -> Task String (List String)
lookupCity query =
  let toUrl =
        if (String.length query >= 4 && String.length query <= 20)
        --then succeed ("http://api.zippopotam.us/us/tx/" ++ query)
        then succeed ("http://api.wunderground.com/api/5d126a693107d7a4/conditions/q/TX/" ++ query ++ ".json")
        else fail "Give me a valid US City!"
  in
      toUrl `andThen` (mapError (always "Not found.") << Http.get places)


places : Json.Decoder (List String)
places =
  let place =
        Json.object1 (\image -> image)
          ("termsofService" := Json.string)
          --("latitude" := Json.string)
         -- ("longitude" := Json.string)
  in
      "response" := Json.list place