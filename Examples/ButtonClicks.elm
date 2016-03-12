import Html exposing (..)
import Html.Events exposing (..)

inbox =
  Signal.mailbox ""

view message =
  div []
    [
      button 
        [ onClick inbox.address "Clicked!" ] 
        [ text "Click On" ],
      button 
        [ onClick inbox.address "" ] 
        [ text "Click Off" ],
      p [ ] [ text message ]
    ]

main =
  Signal.map view inbox.signal