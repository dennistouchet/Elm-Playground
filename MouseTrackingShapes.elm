import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Window


main : Signal Element
main =
  Signal.map2 scene Mouse.position Window.dimensions


scene : (Int,Int) -> (Int,Int) -> Element
scene (x,y) (w,h) =
  let
    (dx,dy) =
      (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
  in
    collage w h
      [ ngon 3 100
          |> filled red
          |> rotate (atan2 dy dx)
      , ngon 6 60
          |> filled green
          |> rotate (degrees 30)
          |> rotate (atan2 dy dx)
      , ngon 5 20
          |> filled blue
          |> move (dx, dy)
      , ngon 3 25
          |> filled purple
          |> move (dx, dy)
          |> rotate (atan2 dy dx)
          |> rotate (degrees 60)
      ]