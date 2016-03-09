module DoubleCounterList where

-- Imports
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import DoubleCounter


-- Model

type alias Model = 
 { counters : List ( ID, DoubleCounter.Model ) 
 , nextID : ID
 }

type alias ID = Int

init : Model
init =
    { counters = []
    , nextID = 0
    }

-- Update

type Action
 = Insert
 | Delete ID
 | Edit ID DoubleCounter.Action
-- TODO: Add clear all Action next

update : Action -> Model -> Model
update action model =
  case action of
    Insert ->
      let newCounter = ( model.nextID, DoubleCounter.init 0 )
          newCounters = model.counters ++ [ newCounter ]
      in
          { model |
              counters = newCounters,
              nextID = model.nextID + 1
          }

    Delete id ->
      { model |
          counters = List.filter (\(counterID, _) -> counterID /= id) model.counters
      }

    Edit id counterAction ->
      let updateCounter (counterID, counterModel) =
            if counterID == id
                then (counterID, DoubleCounter.update counterAction counterModel)
                else (counterID, counterModel)
      in
          { model | counters = List.map updateCounter model.counters }


-- View

view : Signal.Address Action -> Model -> Html
view address model =
  let insert = button [ onClick address Insert ] [ text "Add" ]
  in
      div [] (insert :: List.map (viewCounter address) model.counters)

viewCounter: SignalAddress Action -> (ID, Counter.Model) -> Html
viewCounter =
  let context =
        Counter.Context
          (Signal.forwardTo address (Edit id))
          (Signal.forwardTo address (always (Delete id)))
  in
      DoubleCounter.viewWithRemoveButton context model