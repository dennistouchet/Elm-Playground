import Graphics.Element exposing (..)
import Window
import Time
import Text
import Keyboard


-- MAIN


main : Signal Element
main =
    Signal.map2 view clock Window.dimensions

-- MODEL

type alias Model = Int

init : Model
init = 60


-- UPDATE


type Action = Reset | Increment | Decrement | Tick | DoNothing

update action counter =
  case action of
  
    Reset       -> init
  
    Increment   -> counter + 1
    
    Decrement   -> max 0 (counter - 1)
    
    Tick        ->  max 0 (counter - 1)
    
    DoNothing   -> counter


-- VIEW


view : Int -> (Int, Int) ->  Element
view time (w, h)=
  toString time
  |> Text.fromString
  |> Text.height 40
  |> centered
  |> container w h middle
  --container w h middle (centered (Text.height 50 (Text.fromString (toString time))))


clock : Signal Int
clock =
  Signal.foldp update init actions --foldp (update function) (initial value) (incoming signal)
    
actions : Signal Action
actions =
  Signal.mergeMany [time, input, reset]    

input : Signal Action
input = 
  let
      upDown = Signal.map .y Keyboard.arrows
      toAction y =
        if y == 1 then Increment
        else if y == -1 then Decrement
        else DoNothing
      action = Signal.map toAction upDown
  in
    Signal.sampleOn (Time.fps 30) action

time : Signal Action
time =
    Signal.map (always Tick) (Time.every Time.second)
    
reset : Signal Action
reset =
  Signal.map (always Reset) Keyboard.space
  
-- SUPPORTING FUNCTIONS