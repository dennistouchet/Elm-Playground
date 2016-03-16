import Graphics.Element exposing (..)
import Window
import Time
import Text

time : Signal Time.Time
time =
    Time.every Time.second

clock : Signal Int
clock =
  Signal.foldp update 60 time --foldp (update function) (initial value) (incoming signal)

main : Signal Element
main =
    Signal.map2 view clock Window.dimensions
    
view : Int -> (Int, Int) ->  Element
view time (w, h)=
  toString time
  |> Text.fromString
  |> Text.height 35
  |> centered
  |> container w h middle
  --container w h middle (centered (Text.height 50 (Text.fromString (toString time))))


update _ counter =
    max 0 (counter - 1)