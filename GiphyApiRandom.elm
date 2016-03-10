module GiphyApiRandom where

-- IMPORTS
import StartApp


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

getRandomGif : String -> Effects Action
getRandomGif topic =
	Http.get decodeImageUrl (randomUrl topic)
		|> Task.toMaybe
		|> Task.map NewGif
		|> Effects.task


-- Given a topic, construct a URL for the giphy API.
randomUrl : String -> String
randomUrl topic =
	Http.url "http://api.giphy.com/v1/gifs/random"
	[ "api_key" => "dc6zaTOxFJmzC"
      "tag" => topic
	 ]


-- A JSON decoder that takes a big chunk of JSON spit out by
-- giphy and extracts the string at `json.data.image_url` 
decodeImageUrl : Json.Decode String
decodeImageUrl =
	Json.at ["data", "image_url" ] Json.string

-- VIEW

