import Html                exposing (Html, text, p)
import Signal              exposing (Address)
import Effects             exposing (Effects, Never)
import Json.Decode as Json exposing ((:=))
import StartApp            exposing (start)
import Task
import Http


-- MAIN


app = start
  { init  = init
  , update = update
  , view   = view
  , inputs = []
  }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


-- MODEL


type alias Model = String


init : (Model, Effects Action)
init =
  ("...", getName)


elections : Json.Decoder (List String)
elections =
  let election =
        Json.object3 (\id name electionDay -> id ++ ": " ++ name ++ ", " ++ electionDay)
          ("id" := Json.string)
          ("name" := Json.string)
          ("electionDay" := Json.string)
  in
      "elections" := Json.list election

userUrl : String
userUrl = "https://www.googleapis.com/civicinfo/v2/elections?key=AIzaSyCiNxthpYuUUCebKAfeqVoaFQqhogQhOBA"


-- UPDATE


type Action = NewUser (Maybe String)

update : Action -> Model -> (Model, Effects Action)
update action model =
  let name maybeName = Maybe.withDefault "error" maybeName
  in
    case action of
      NewUser maybeName -> (name maybeName, Effects.none)


-- VIEW


view : Address Action -> Model -> Html
view address model =
  p [] [ text model ]


-- EFFECTS


getName : Effects Action
getName =
  Http.get decodeName userUrl
    |> Task.toMaybe
    |> Task.map NewUser
    |> Effects.task