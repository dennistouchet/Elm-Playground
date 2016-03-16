import Char
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (..)

-- MAIN


main =
 Signal.map2 view query.signal results.signal


-- VIEW

view : String -> Result String (List String) -> Html
view string result =
  let field =
        input
          [ placeholder "Enter the word USERS"
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

-- EFFECTS 

query : Signal.Mailbox String
query =
  Signal.mailbox ""


results : Signal.Mailbox (Result String (List String))
results =
  Signal.mailbox (Err "Invalid")


port requests : Signal (Task x ())
port requests =
  Signal.map lookupCity query.signal
    |> Signal.map (\task -> Task.toResult task `andThen` Signal.send results.address)


lookupCity : String -> Task String (List String)
lookupCity query =
  let toUrl =
        if (String.length query >= 4 && String.length query <= 6)
        then succeed ("http://jsonplaceholder.typicode.com/" ++ query)
        else fail "Invalid query"
  in
      toUrl `andThen` (mapError (always "Not found.") << Http.get users)


users : Json.Decoder (List String)
users =
    let user =
        Json.object3 (\id username phone -> id ++ ": " ++ username ++ ", " ++ phone)
          ("id" := Json.string)
          ("username" := Json.string)
          ("phone" := Json.string)
  in
        "" := Json.list user