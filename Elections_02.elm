module Elections_02 where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Char
import String
import Task exposing (..)


-- MAIN


main =
   Signal.map2 view query.signal results.signal


port requests : Signal (Task x ())
port requests =
  Signal.map getElections query.signal
    |> Signal.map (\task -> Task.toResult task `andThen` Signal.send results.address)


-- MODEL

type alias Model = 
      { url : String
      , elections : List String
      }

init : (Model, Effects Action)
init =
    ( 
      { url = ""
      , elections = ["..."] 
      }
      , getElection
    )


-- UPDATE

type Action = Request | NewElection ( Maybe (List String ))

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
    Request ->
      (model, getElection)
    
    NewElection maybeUrl -> 
        ( Model model.url (Maybe.withDefault model.elections maybeUrl)
        , Effects.none
        )
    

query : Signal.Mailbox String
query =
  Signal.mailbox ""


results : Signal.Mailbox (Result String (List String))
results =
  Signal.mailbox (Err "A valid US City contain only letters.")


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


-- HELPERS

--getElection : String -> Effects Action
getElection =
  Http.get elections (electionUrl)
    |> Task.toMaybe
    |> Task.map NewElection
    |> Effects.task
  

getElections : String -> Task String (List String)
getElections query =
  let toUrl =
        if (String.length query >= 4 && String.length query <= 20) && not (String.all Char.isDigit query)
          then succeed ("https://www.googleapis.com/civicinfo/v2/elections?fields=elections(electionDay%2Cid%2Cname)&key=AIzaSyCiNxthpYuUUCebKAfeqVoaFQqhogQhOBA")
          else fail "Give me a valid US City!"
  in
      toUrl `andThen` (mapError (always "Not found.") << Http.get elections)


(=>) = (,)

electionUrl : String
electionUrl =
    Http.url "https://www.googleapis.com/civicinfo/v2/elections"
    [ "fields" => "elections(electionDay%2Cid%2Cname)"
    , "key" => "AIzaSyCiNxthpYuUUCebKAfeqVoaFQqhogQhOBA"
    ]


elections : Json.Decoder (List String)
elections =
  let election =
        Json.object3 (\id name electionDay -> id ++ ": " ++ name ++ ", " ++ electionDay)
          ("id" := Json.string)
          ("name" := Json.string)
          ("electionDay" := Json.string)
  in
      "elections" := Json.list election