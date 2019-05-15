module Rack exposing (Rack, empty, init, take, return, replenish, view)

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

type Rack =
  R (Array Cell)

size : Int
size = 7

empty : Rack
empty = R (Array.repeat size Empty)

-- assumes bag has over seven tiles
init : List Tile -> (Rack, List Tile)
init bag =
  ( bag
    |> List.take size
    |> List.map (Occupied False)
    |> Array.fromList
    |> R
  , List.drop size bag
  )

take : Int -> Rack -> (Maybe (Int, Tile), Rack)
take index (R cells) =
  case Array.get index cells of
    Nothing    -> (Nothing, R cells)
    Just Empty -> (Nothing, R cells)
    Just (Occupied chosen tile) ->
      case chosen of
        True ->
          (Nothing, R cells)
        False ->
          (Just (index, tile), R (Array.set index (Occupied True tile) cells))

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

replenish : List Tile -> Rack -> (Rack, List Tile)
replenish bag (R cells) =
  Array.foldl
    (\cell (newCells, newBag) ->
      case cell of
        Empty ->
          (Array.push cell newCells, newBag)
        Occupied False tile ->
          (Array.push cell newCells, newBag)
        Occupied True tile ->
          case List.head newBag of
            Nothing ->
              (Array.push Empty newCells, newBag)
            Just newTile ->
              (Array.push (Occupied False newTile) newCells, List.drop 1 newBag)
    )
    (Array.empty, bag) cells
  |> Tuple.mapFirst R

viewCell : msg -> Cell -> Html msg
viewCell event cell =
  let
    (attr, html) =
      case cell of
        Empty ->
          ( [ class "empty" ]
          , []
          )
        Occupied chosen tile ->
          ( if chosen then
              [ onClick event, class "chosen" ]
            else
              [ onClick event ]
          , [ Tile.view tile ]
          )
  in
    Html.div attr html

view : (Int -> msg) -> Rack -> Html msg
view event (R cells) =
  Html.div
  [ class "rack" ]
  (cells
    |> Array.indexedMap (\i -> viewCell (event i))
    |> Array.toList)