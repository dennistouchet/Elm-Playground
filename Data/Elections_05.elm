module Elections_05 where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Char
import String
import Task exposing (..)
import StartApp


-- START APP

app =
    StartApp.start
        { init = init 
        , update = update
        , view = view
        , inputs = []
        }

-- MAIN

main =
    app.html

port tasks : Signal (Task.Task Never())
port tasks =
    app.tasks

-- MODEL

type alias Model = 
      { url : String
      , elections : List String
      }

init : (Model, Effects Action)
init =
    ( 
      { url = "Google Civic Info Election Data "--electionUrl
      , elections = ["..."] 
      }
      , getElection
    )

-- UPDATE

type Action = Request | NewElection (Maybe (List String ))

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
    Request ->
      (model, getElection)
    
    NewElection els -> 
        ( Model model.url (Maybe.withDefault model.elections els)
        , Effects.none
        )

-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  let 
      field = 
        div [ myStyle ] [ h1 [ ] [text model.url] ]
      
      messages =
        List.map (\election -> div [ dataStyle ] [ text election ] ) model.elections
  in
    div [] [div [gridStyle] (field :: messages)]

myStyle : Attribute
myStyle =
  style
    [ ("width", "90%")
    , ("height", "40px")
    , ("margin", "0 0 40px 0") 
--    , ("padding", "10px 5xp 5px 5px")
--    , ("font-size", "2em")
    , ("text-align", "center")
    ]

dataStyle : Attribute
dataStyle =
  style
    [ ("width", "95%")
    , ("height", "40px")
    , ("padding", "1% 2% 0 2%")
    , ("font-size", "20px")
    , ("text-align", "left")
    , ("border-style", "solid solid none solid")
    ]

gridStyle : Attribute
gridStyle =
  style
    [ ("margin", "0 40px 0 40px") 
    , ("border-style", "none none solid none") ]

-- HELPERS

getElections : String -> Task String (List String)
getElections query =
  let toUrl =
        if (String.length query >= 4 && String.length query <= 20) && not (String.all Char.isDigit query)
          then succeed ("https://www.googleapis.com/civicinfo/v2/elections?fields=elections(electionDay%2Cid%2Cname)&key=AIzaSyCiNxthpYuUUCebKAfeqVoaFQqhogQhOBA")
          else fail "Give me a valid US City!"
  in
      toUrl `andThen` (mapError (always "Not found.") << Http.get elections)


--getElection : String -> Effects Action 
getElection =
    electionUrl
      |> Http.get elections
      |> Task.toMaybe
      |> Task.map NewElection
      |> Task.mapError (Debug.log "ERROR")
      |> Effects.task
    
(=>) = (,)


electionUrl : String
electionUrl =
    Http.url "https://www.googleapis.com/civicinfo/v2/elections"
    [ "key" => "AIzaSyCiNxthpYuUUCebKAfeqVoaFQqhogQhOBA"
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