import Graphics.Element exposing (show)

main =
  show (length [1..27940]) -- 27937 +1 27940 is maximum call stack length

length : List a -> Int
length list =
  case list of
    [] -> 0        
    first :: rest ->
        1 + length rest