module Rack exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)

import Tile exposing (Tile)

type alias Chosen
  = Bool

type Cell
  = Empty
  | Occupied Chosen Tile

type Rack = R (Array Cell)

size : Int
size = 7

empty : Rack
empty = R (Array.repeat size Empty)

init = R (Array.map (Occupied False << Tile.letter) <| Array.fromList <| String.toList "abcdefg")

take : Int -> Rack -> (Maybe Tile, Rack)
take index (R cells) =
  case Array.get index cells of
    Nothing    -> (Nothing, R cells)
    Just Empty -> (Nothing, R cells)
    Just (Occupied chosen tile) ->
      case chosen of
        True ->
          (Nothing, R cells)
        False ->
          (Just tile, R (Array.set index (Occupied True tile) cells))

return : Int -> Rack -> Rack
return index (R cells) =
  case Array.get index cells of
    Nothing    -> R cells
    Just Empty -> R cells
    Just (Occupied chosen tile) ->
      case chosen of
        True ->
          R (Array.set index (Occupied False tile) cells)
        False ->
          R cells

replenish : Rack -> List Tile -> (Rack, List Tile)
replenish (R spots) bag =
  Debug.todo "TODO"

viewCell : (Int -> msg) -> Int -> Cell -> Html msg
viewCell event i cell =
  let
    (attr, html) =
      case cell of
        Empty ->
          ( [ class "empty" ]
          , []
          )
        Occupied chosen tile ->
          ( if chosen then
              [ class "chosen" ]
            else
              [ onClick (event i) ]
          , [ Tile.view tile ]
          )
  in
    Html.div attr html

view : (Int -> msg) -> Rack -> Html msg
view event (R cells) =
  Html.div
  [ class "rack" ]
  (Array.toList <| Array.indexedMap (viewCell event) cells)