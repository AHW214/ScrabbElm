module Rack exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes exposing (class)

import Tile exposing (Tile, Status(..))

type Spot
  = Empty
  | Occupied Status Tile

type Rack = R (Array Spot)

size : Int
size = 7

empty : Rack
empty = R Array.empty

init = R (Array.map (Occupied Placed << Tile.letter) <| Array.fromList <| String.toList "abcdefg")

-- silly
take : Int -> Rack -> Rack
take index (R spots) =
  case Array.get index spots of
    Nothing    -> R spots
    Just Empty -> R spots
    Just (Occupied _ tile) ->
      R (Array.set index (Occupied Held tile) spots)

replenish : Rack -> List Tile -> (Rack, List Tile)
replenish (R spots) bag =
  Debug.todo "TODO"

viewSpot : (Int -> msg) -> Int -> Spot -> Html msg
viewSpot event i spot =
  case spot of
    Empty ->
      Html.div [ class "empty" ] []
    Occupied status tile ->
      Tile.view (event i) status tile

view : (Int -> msg) -> Rack -> Html msg
view event (R spots) =
  Html.div
  [ class "rack" ]
  (Array.toList <| Array.indexedMap (viewSpot event) spots)