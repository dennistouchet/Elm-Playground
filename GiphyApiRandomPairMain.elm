import Effects exposing (Never)
import GiphyApiRandomPair exposing (init, update, view)
import StartApp
import Task


app =
    StartApp.start
        { init = init "programming" "computers"
        , update = update
        , view = view
        , inputs = []
        }

main =
    app.html


port tasks : Signal (Task.Task Never())
port tasks =
    app.tasks