module GiphyApiRandom where

-- IMPORTS
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json
import Task


--MODEL

type alias Model =
 { topic : String
 , gifUrl : String
 }

init : String -> (Model, Effects Action)
init top =
 ( Model top "asset/waiting.gif"
 , getRandomGif top
 )

-- UPDATE

type Action = RequestMore | NewGif (Maybe String)

update : Action -> Model -> (Model, Effects Action)
update msg model =
  case msg of
    RequestMore ->
     ( model
     , getRandomGif model.topic
     )

    NewGif maybeUrl ->
     ( Model model.topic (Maybe.withDefault model.gifUrl maybeUrl)
     , Effects.none
     )

-- VIEW

(=>) = (,)


view : Signal.Address Action -> Model -> Html
view address model =
  div [ style [ "width" => "200px" ] ]
    [ h2 [headerStyle] [text model.topic]
    , div [imgStyle model.gifUrl] []
    , button [ onClick address RequestMore ] [ text "More Please!" ]
    ]


headerStyle : Attribute
headerStyle =
  style
    [ "width" => "200px"
    , "text-align" => "center"
    ]


imgStyle : String -> Attribute
imgStyle url =
  style
    [ "display" => "inline-block"
    , "width" => "200px"
    , "height" => "200px"
    , "background-position" => "center center"
    , "background-size" => "cover"
    , "background-image" => ("url('" ++ url ++ "')")
    ]

-- EFFECTS

getRandomGif : String -> Effects Action
getRandomGif topic =
    Http.get decodeImageUrl (randomUrl topic)
        |> Task.toMaybe
        |> Task.map NewGif
        |> Effects.task
    --Effects.task ( Task.map NewGif ( Task.toMaybe (Http.get decodeImageUrl (randomUrl topic)))) --alternative


-- Given a topic, construct a URL for the giphy API.
randomUrl : String -> String
randomUrl topic =
    Http.url "http://api.giphy.com/v1/gifs/random"
    [ "api_key" => "dc6zaTOxFJmzC"
    , "tag" => topic
    ]


-- A JSON decoder that takes a big chunk of JSON spit out by
-- giphy and extracts the string at `json.data.image_url` 
decodeImageUrl : Json.Decoder String
decodeImageUrl =
    Json.at ["data", "image_url" ] Json.string