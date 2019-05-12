module Rack exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes exposing (class)

import Events exposing (Msg(..))
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
take : Int -> Rack -> (Maybe Tile, Rack)
take index (R spots) =
  case Array.get index spots of
    Nothing    -> (Nothing, R spots)
    Just Empty -> (Nothing, R spots)
    Just (Occupied _ tile) ->
      (Just tile, R (Array.set index (Occupied Held tile) spots))

replenish : Rack -> List Tile -> (Rack, List Tile)
replenish (R spots) bag =
  Debug.todo "TODO"

viewSpot : Int -> Spot -> Html Msg
viewSpot i spot =
  case spot of
    Empty ->
      Html.div [ class "empty" ] []
    Occupied status tile ->
      Tile.view (ChoseTile i) status tile

view : Rack -> Html Msg
view (R spots) =
  Html.div
  [ class "rack" ]
  (Array.toList <| Array.indexedMap viewSpot spots)