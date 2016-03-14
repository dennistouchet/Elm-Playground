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
  ("...", getUser)


decodeUser : Json.Decoder String
decodeUser =
        Json.object3 (\id username phone -> toString id ++ "," ++ username ++ "," ++ phone)
          ("id" := Json.int)
          ("username" := Json.string)
          ("phone" := Json.string)


userUrl : String
userUrl = "http://jsonplaceholder.typicode.com/users/1"


-- UPDATE


type Action = NewUser (Maybe String)

update : Action -> Model -> (Model, Effects Action)
update action model =
  let user maybeUser = Maybe.withDefault "error" maybeUser
  in
    case action of
      NewUser maybeUser -> (user maybeUser, Effects.none)


-- VIEW


view : Address Action -> Model -> Html
view address model =
  p [] [ text model ]


-- EFFECTS


getUser : Effects Action
getUser =
  Http.get decodeUser userUrl
    |> Task.toMaybe
    |> Task.map NewUser
    |> Effects.task
